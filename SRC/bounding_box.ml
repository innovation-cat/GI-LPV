open Vector

type bounding_box = {mutable min : vec; mutable max : vec}

let create () = {min = Vec3 (0.0,0.0,0.0); max = Vec3 (0.0,0.0,0.0)}

let calc_center bbox = 
	match (bbox.min, bbox.max) with
	    (Vec3 (x1,y1,z1), Vec3 (x2,y2,z2)) -> Vec3 ((x1 +. x2) /. 2.0, (y1 +. y2) /. 2.0, (z1 +. z2) /. 2.0)
	|   _ -> raise (Failure "bounding_box: unexpected error.")

let add_vertex (x,y,z) bbox = 
	match (bbox.min, bbox.max) with
	    (Vec3 (x1,y1,z1), Vec3 (x2,y2,z2)) -> bbox.min <- Vec3 ( min x x1 , min y y1 , min z z1);
						  bbox.max <- Vec3 ( max x x2 , max y y2 , max z z2);
						  bbox
	|  _  -> raise (Failure "bounding_box: unexpected error.")

let calc_dim bbox = 
	match (bbox.min, bbox.max) with
	    (Vec3 (x1,y1,z1), Vec3 (x2,y2,z2)) -> Vec3 ((x2 -. x1) /. 2.0, (y2 -. y1) /. 2.0, (z2 -. z1) /. 2.0)
	|   _ -> raise (Failure "bounding box: unexpected error.")
