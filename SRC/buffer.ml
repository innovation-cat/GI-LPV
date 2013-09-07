open VBO

type position = {x : float; y : float; z : float};;

type normal = {m : float; n : float; k : float};;

type texture_coord = { s : float; t : float};;

type vertex = { pos : position;
		nor : texture_coord;
		tc  : texture_coord
	      }

type vertex_buffer = { name : string; vbo : vbo_id}


