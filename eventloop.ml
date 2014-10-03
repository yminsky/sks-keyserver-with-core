(***********************************************************************)
(* eventloop.ml - Basic eventloop for picking up timer and socket      *)
(*                events                                               *)
(*                                                                     *)
(* Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, *)
(*               2011, 2012, 2013  Yaron Minsky and Contributors       *)
(*                                                                     *)
(* This file is part of SKS.  SKS is free software; you can            *)
(* redistribute it and/or modify it under the terms of the GNU General *)
(* Public License as published by the Free Software Foundation; either *)
(* version 2 of the License, or (at your option) any later version.    *)
(*                                                                     *)
(* This program is distributed in the hope that it will be useful, but *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *)
(* General Public License for more details.                            *)
(*                                                                     *)
(* You should have received a copy of the GNU General Public License   *)
(* along with this program; if not, write to the Free Software         *)
(* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 *)
(* USA or see <http://www.gnu.org/licenses/>.                          *)
(***********************************************************************)

open Core.Std
open Common
open Packet


(** Timeout code.
    Allows the addition of generic timeouts for actions *)

exception SigAlarm
let waiting_for_alarm = ref false
let sigalarm_handler _ =
  if !waiting_for_alarm
  then raise SigAlarm
  else ()

let _ =
  Signal.Expert.handle Signal.alrm sigalarm_handler

type timed_event =
    Event of float * callback
and timed_callback = { callback: unit -> timed_event list;
                       timeout: int;
                       name: string option;
                     }
and callback = | Callback of (unit -> timed_event list)
               | TimedCallback of timed_callback


type timed_handler =
  { h_callback: Unix.sockaddr -> in_channel -> out_channel -> timed_event list;
    h_timeout: int;
    h_name: string option;
  }
type handler =
  | Handler of (Unix.sockaddr -> in_channel -> out_channel -> timed_event list)
  | TimedHandler of timed_handler


let unwrap opt = match !opt with
    None -> failwith "unwrap failure"
  | Some x -> x

let make_tc ~name ~timeout ~cb =
  TimedCallback { callback = cb;
                  name = Some name;
                  timeout = timeout;
                }

let make_th ~name ~timeout ~cb =
  TimedHandler { h_callback = cb;
                 h_name = Some name;
                 h_timeout = timeout;
               }

(** reraises an exception if it is a user-initiated break or a SigAlarm *)
let reraise e = match e with
    Sys.Break | SigAlarm -> raise e
  | _ -> ()

(*************************************************************)

(** executes function with timeout enforced using Unix.alarm *)
let do_with_timeout f timeout =
  ignore (Unix.alarm timeout);
  waiting_for_alarm := true;
  protect ~f
    ~finally:(fun () ->
      waiting_for_alarm := false;
      ignore (Unix.alarm 0);)


let cbname cb = match cb.name with
    None -> ""
  | Some s -> sprintf "<%s> " s


(** Does timed callback, including possible recovery action,
    with timeouts enforced by Unix.alarm *)
let do_timed_callback cb =
  try
    do_with_timeout cb.callback cb.timeout
  with
  | Sys.Break as e ->
    perror "%scallback interrupted by break." (cbname cb);
    raise e
  | SigAlarm ->
    perror "%scallback timed out." (cbname cb);
    []
  | e ->
    eplerror 2 e "%serror in callback." (cbname cb);
    []

let do_callback cb = match cb with
  | TimedCallback cb -> do_timed_callback cb
  | Callback cb -> cb ()


(** Socket handling functions *)

let create_sock addr =
  try
    let domain =
      Unix.domain_of_sockaddr addr in
    let sock =
      Unix.socket ~domain ~kind:SOCK_STREAM ~protocol:0 in
    Unix.setsockopt sock SO_REUSEADDR true;
    if domain = PF_INET6 then
      Unix.setsockopt sock IPV6_ONLY true;
    Unix.bind sock ~addr;
    Unix.listen sock ~max:20;
    sock
  with
  | Unix.Unix_error (_,"bind",_) ->
    failwith "Failure while binding socket.  \
              Probably another socket bound to this address"
  | e -> raise e
;;

let add_events heap evlist =
  List.iter evlist ~f:(fun (Event (time, callback)) ->
    Heap.add heap (time,callback))

let maybe_create_sock addr =
  try
    Some (create_sock addr)
  with
  | err ->
    let saddr = match addr with
      | ADDR_UNIX path ->  "\"" ^ path ^ "\""
      | ADDR_INET(ip, port) ->
        Unix.Inet_addr.to_string ip ^ ":" ^ Int.to_string port
    in
    perror "Failed to listen on %s: %s" saddr (err_to_string err);
    None

(***************************************************************)
(*  Event Handlers  *******************************************)
(***************************************************************)

let handle_socket handler sock =
  let (s,caller) = Unix.accept sock in
  let inchan = Unix.in_channel_of_descr s in
  let outchan = Unix.out_channel_of_descr s in
  protect ~f:(fun () -> handler caller inchan outchan)
    ~finally:(fun () -> Unix.close s)


let handler_to_callback handler sock =
  match handler with
    Handler handler ->
    Callback (fun () ->
      let (s,caller) = Unix.accept sock in
      let inchan = Unix.in_channel_of_descr s in
      let outchan = Unix.out_channel_of_descr s in
      protect ~f:(fun () -> handler caller inchan outchan)
        ~finally:(fun () -> Unix.close s)
    )
  | TimedHandler handler ->
    TimedCallback
      { callback =
          (fun () ->
             let (s,caller) = Unix.accept sock in
             let inchan = Unix.in_channel_of_descr s
             and outchan = Unix.out_channel_of_descr s in
             protect ~f:(fun () -> handler.h_callback
                                     caller inchan outchan)
               ~finally:(fun () -> Unix.close s)
          );
        timeout = handler.h_timeout;
        name = handler.h_name;
      }

(***************************************************************)
(*  Event Loop  ***********************************************)
(***************************************************************)

let some opt = match opt with
    None -> false
  | Some x -> true

(***************************************************************)

(** Does all events occuring at or before time [now], updating heap
    appropriately.  Returns the time left until the next undone event
    on the heap
*)
let rec do_current_events heap now =
  match Heap.top heap with
  | Some (time,callback) ->
    let timeout = time -. now in
    if timeout <= 0.0 then (
      ignore (Heap.pop heap);
      add_events heap (do_callback callback);
      do_current_events heap now;
    ) else timeout
  | None -> -1.0

(** function for adding to heap callbacks for handling
    incoming socket connections *)
let add_socket_handlers heap now fdlist sockets =
  List.iter sockets
    ~f:(fun sock ->
      try
        let handler = List.Assoc.find_exn fdlist sock in
        add_events heap
          [ Event (now, handler_to_callback handler sock) ]
      with
        Not_found ->
        plerror 0 "%s" ("BUG: eventloop -- socket without " ^
                        "handler.  Event dropped")
    )
(** Do all available events in FIFO order *)
let do_next_event heap fdlist =
  let now = Unix.gettimeofday () in
  let timeout = `After (do_current_events heap now) in
  let (fds,_) = List.unzip fdlist in
  let selected = 
    Unix.select ~read:fds ~write:[] ~except:[] ~timeout ()
  in
  add_socket_handlers heap now fdlist selected.read

(***************************************************************)
(***************************************************************)

let heap = 
  Heap.create ~cmp:(fun (t1,_) (t2,_) -> compare t1 t2) ()

let evloop events socklist =
  add_events heap events;
  try
    while true do
      try
        do_next_event heap socklist
      with
      | Sys.Break ->
        eprintf "Ctrl-C.  Exiting eventloop\n";
        flush Pervasives.stderr;
        raise Exit
      | Unix.Unix_error (error,func_name,param) ->
        if error <> Unix.EINTR
        (* EINTR just means the alarm interrupted select *)
        then
          plerror 2 "%s" ("eventloop: Unix Error: " ^
                          (Unix.error_message error) ^ ",  " ^
                          func_name ^ ", " ^ param ^ "\n")
      | e -> eplerror 2 e "eventloop"
    done
  with
    Exit -> ()
