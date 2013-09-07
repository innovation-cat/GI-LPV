module Resource_Map = Map.Make (struct type t = string let compare = String.compare end)

module rec Resource_List : sig 
	(*type 'a value = 'a Resource_Base.t*)
	
	type 'a t = {mutable resource_map : 'a Resource_Map.t}

	val empty_map : 'a t

	val get : key:string -> 'a t -> 'a

	val element_exists : key:string -> 'a t -> bool

	val check_name : key:string -> 'a t -> bool

	val insert : key:string -> value:'a -> 'a t -> unit

	val remove : key:string -> 'a t -> unit 
	
	val remove_all : 'a t -> unit
end = struct 
	(*type 'a value = 'a Resource_Base.t*)

	type 'a t = {mutable resource_map : 'a Resource_Map.t}

	let empty_map = {resource_map = Resource_Map.empty}

	let get ~key d = Resource_Map.find key d.resource_map

	let element_exists ~key d = Resource_Map.mem key d.resource_map

	let check_name ~key d = if String.length key = 0 || element_exists ~key d then false
				else true

	let insert ~key ~value d = d.resource_map <- Resource_Map.add key value d.resource_map; 
				   let v = get ~key d in Resource_Base.set_resource_list d v

	let remove ~key d = d.resource_map <- Resource_Map.remove key d.resource_map

	let remove_all d = d.resource_map <- Resource_Map.empty
end
and Resource_Base : sig 
	type 'a t = {
			mutable ref_counter : int;
			mutable name        : string;
			mutable res_list    : 'a Resource_List.t
		    }

	val init : ?name:string -> ?ref_counter:int -> ?res_list:'a Resource_List.t -> 'b -> 'a t

	val get_ref : 'a t -> int
	
	val get_name : 'a t -> string

	val ref : 'a t -> unit

	val unref : 'a t -> unit

	val set_resource_list : 'a Resource_List.t -> 'a t -> unit
end = struct 
	type 'a t = {
			mutable ref_counter : int;
			mutable name        : string;
			mutable res_list    : 'a Resource_List.t
		    }

	let init ?(name="") ?(ref_counter=0) ?(res_list={Resource_List.resource_map = Resource_List.empty_map}) r =
							   {name; ref_counter; res_list}

	let get_ref d = d.ref_counter

	let get_name d = d.name

	let ref d = d.ref_counter <- d.ref_counter + 1

	let unref d = if d.ref_counter = 0 then raise (Failure "ref counter already zero.");
		      d.ref_counter <- d.ref_counter - 1;
		      if Resource_Map.is_empty d.res_list.Resource_List.resource_map then raise (Failure "error, resource list is null.");
		      Resource_List.remove ~key:d.name d.res_list

	let set_resource_list s d = d.res_list <- s
end
