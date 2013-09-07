open VBO

type position = {x : float; y : float; z : float};;

type normal = {m : float; n : float; k : float};;

type texture_coord = { s : float; t : float};;

type vertex = { pos : position;
		nor : normal;
		tc  : texture_coord
	      }

type field_type = VEC3F | VEC2F | FLOAT | VEC4F | MAT4X4 | UNSIGNED_INTEGER | RGB | LAST_FIELD

type declaration = { field : field_type; name : string}

type vertex_buffer = {
			mutable vbo          : vbo_id;
			mutable name         : string;
			mutable length 	     : int;
			mutable element_size : int;
			mutable buffer_decl  : declaration array;
		     }

module Resource_Map = Map.Make (struct type t = string let compare = String.compare end);;

let buffer_list = ref (Resource_Map.empty);;

let insert_vertex_buffer buffer = buffer_list := Resource_Map.add buffer.name buffer (!buffer_list)
	
let parse ~line =
    let size = String.length line in 
    let rec aux i isforward = 
      if line.[i]=' ' || line.[i]='\t' then 
        if isforward then aux (i+1) isforward
        else aux (i-1) isforward
      else i
    in
    let left = aux 0 true in let right =  aux (size-1) false in 
    (String.sub line left (right-left+1))
;;

(* remove white line and comment line *)
let read_str ~ic = 
    let rec aux () = 
      let line = input_line ic in 
         if line="" then aux ()
         else if line.[0]='/' then aux ()
         else parse line 
    in
    aux ()
;;

let setup ~ic = 
    let trianglenum = 
      let 
        line = read_str ~ic 
      in Scanf.sscanf line "NUMPOLLIES %d" (fun x -> x) 
    in 
    let vertexes = Array.init trianglenum (fun i ->
        let create_vertex () = 
            let line = read_str ~ic in 
            Scanf.sscanf line "%f %f %f %f %f %f %f %f" (fun x y z m n k s t -> {pos={x; y; z}; nor={m; n; k}; tc={s; t}})
        in 
        create_vertex () )
    in
    vertexes
;;
