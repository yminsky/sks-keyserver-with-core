open Core.Std

module F :
  functor (M : sig  end) ->
    sig
      val settings : PTreeDB.ptree_settings
      val reconsocks : Unix.File_descr.t list
      val comsock : Unix.File_descr.t
      val filters : string list option ref
      val get_filters : unit -> string list
      val eventify_handler :
        ('a -> Channel.sys_in_channel -> Channel.sys_out_channel -> 'b) ->
        'a -> in_channel -> out_channel -> 'b
      val choose_partner : unit -> Unix.addr_info
      val missing_keys_timeout : int
      val get_missing_keys : unit -> Eventloop.timed_event list
      val sockaddr_to_name : Unix.sockaddr -> string
      val recon_handler :
        Unix.sockaddr ->
        in_channel -> out_channel -> Eventloop.timed_event list
      val initiate_recon : unit -> Eventloop.timed_event list
      val command_handler :
        'a ->
        < upcast : Channel.in_channel_obj; .. > ->
        < flush : 'b; upcast : Channel.out_channel_obj; .. > ->
        Eventloop.timed_event list
      val sync_interval : float
      val sync_tree : unit -> unit
      val checkpoint_interval : float
      val prepare : unit -> unit
      val run : unit -> unit
    end
