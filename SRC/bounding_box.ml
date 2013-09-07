type vec3 = { mutable x : float; mutable y : float; mutable z : float}

type bounding_box = {mutable min : vec3; mutable max : vec3}

let calc_center bbox = {
			x = (bbox.min.x +. bbox.max.x) /. 2.0; 
			y = (bbox.min.y +. bbox.max.y) /. 2.0;
			z = (bbox.min.y +. bbox.max.y) /. 2.0;
		       }

let add_vertex v bbox = 
	if v.x < bbox.min.x then bbox.min.x <- v.x
	else if v.y < bbox.min.y then bbox.min.y <- v.y
	else if v.z < bbox.min.z then bbox.min.z <- v.z
	else if v.x > bbox.max.x then bbox.max.x <- v.x
	else if v.y > bbox.max.y then bbox.max.y <- v.x
	else if v.z > bbox.max.z then bbox.max.z <- v.z
;;

let calc_dim bbox = {
			x = bbox.max.x -. bbox.min.x;
		    	y = bbox.max.y -. bbox.min.y;
			z = bbox.max.z -. bbox.min.z;
		     }
