type state = IDLE | FORWARD | BACKWARD | RIGHT | LEFT | UP | DOWN | MOVE_UP | MOVE_DOWN | MOVE_LEFT | MOVE_RIGHT

type t = {
		mutable pos           : Vector.vec;
		mutable rot           : Vector.vec;
		mutable move          : state;
		mutable dirty 	      : bool;
		mutable cached_matrix : float array;
	 }

let pi = 4.0 *. (atan 1.0);;

let (|>) x f = f x;;

let get_transform cam frame_duration =
	if cam.dirty = false then cam.cached_matrix
	else begin  
		let Vector.Vec3 (rotx,roty,rotz) = cam.rot in
		let rotation = Vector.build_rotation_mat_1 rotx roty rotz in
		let zaxis = Vector.get_z_axis rotation in
		let xaxis = Vector.get_x_axis rotation in
		let yaxis = Vector.Vec3 (0.0, 1.0, 0.0) in
		begin
			match cam.move with
				   IDLE -> ()
				|  RIGHT -> let y = roty -. ((frame_duration /. 5000.) *. pi) in cam.rot <- Vector.Vec3 (rotx, y, rotz) 
				|  LEFT -> let y = roty +. ((frame_duration /. 5000.) *. pi) in cam.rot <- Vector.Vec3 (rotx, y, rotz)
				|  UP -> let x = rotx +. ((frame_duration /. 10000.) *. pi) in cam.rot <- Vector.Vec3 (x, roty, rotz)
				|  DOWN -> let x = rotx -. ((frame_duration /. 10000.) *. pi) in cam.rot <- Vector.Vec3 (x, roty, rotz)
				|  FORWARD -> let new_pos = (Vector.scale zaxis (frame_duration /. 200.) |> Vector.add cam.pos) in cam.pos <- new_pos  
				|  BACKWARD -> let new_pos = (Vector.scale zaxis (frame_duration /. 200.) |> Vector.sub cam.pos) in cam.pos <- new_pos  
				|  MOVE_UP -> let new_pos = (Vector.scale yaxis (frame_duration /. 200.) |> Vector.sub cam.pos) in cam.pos <- new_pos  
				|  MOVE_DOWN -> let new_pos = (Vector.scale yaxis (frame_duration /. 200.) |> Vector.add cam.pos) in cam.pos <- new_pos  
				|  MOVE_LEFT -> let new_pos = (Vector.scale xaxis (frame_duration /. 200.) |> Vector.add cam.pos) in cam.pos <- new_pos  
				|  MOVE_RIGHT -> let new_pos = (Vector.scale xaxis (frame_duration /. 200.) |> Vector.sub cam.pos) in cam.pos <- new_pos
		end;
		let translation = Vector.set_translation (Vector.build_identity_matrix ()) cam.pos in
		let Vector.Vec3 (rotx,roty,rotz) = cam.rot in
		let rotation = Vector.build_rotation_mat_1 rotx roty rotz in
		let cached = Vector.mult rotation translation in
		cam.cached_matrix <- cached;
		cached
	    end
;;   

let init_camera = let cam = { pos = Vector.Vec3 (-8.5, -0.67, 0.9);
		    	      rot = Vector.Vec3 (-0.008, 1.47, 0.0);
			      move = IDLE;
			      dirty = true;
			      cached_matrix = Array.make 16 0.0; }
		  in
		  get_transform cam 0.0;
		  cam;
