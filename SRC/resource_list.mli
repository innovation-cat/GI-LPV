module Resource_Map :
  sig
    type key = string
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
module rec Resource_List :
  sig
    type 'a value = 'a Resource_Base.t
    type 'a t = { mutable resource_map : 'a value Resource_Map.t; }
    val empty_map : 'a value Resource_Map.t
    val get : key:string -> 'a t -> 'a value
    val element_exists : key:string -> 'a t -> bool
    val check_name : key:string -> 'a t -> bool
    val insert : key:string -> value:'a value -> 'a t -> unit
    val remove : key:string -> 'a t -> unit
    val remove_all : 'a t -> unit
  end
and Resource_Base :
  sig
    type 'a t = {
      mutable ref_counter : int;
      mutable name : string;
      mutable res_list : 'a Resource_List.t;
    }
    val init :
      ?name:string ->
      ?ref_counter:int -> ?res_list:'a Resource_List.t -> 'b -> 'a t
    val get_ref : 'a t -> int
    val get_name : 'a t -> string
    val ref : 'a t -> unit
    val unref : 'a t -> unit
    val set_resource_list : 'a Resource_List.t -> 'a t -> unit
  end
