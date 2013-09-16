open Vector
open Bounding_box

type space = {
		mutable rotation    : float array;  (* rotation matrix *)
		mutable translation : float array;  (* translation matrix *)
		mutable projection  : float array;  (* projection matrix *)
	     }

type light = {
		mutable dir        : vec;
		mutable grid_bbox  : bounding_box;
	 	mutable grid_space : space;
	     }

let (|>) x f = f x

let init = { dir = Vector.Vec3 (0.0, 0.0, 1.0);
	     grid_bbox = Bounding_box.init;
	     grid_space = { rotation = Vector.build_identity_matrix (); 
			    translation = Vector.build_identity_matrix (); 
			    projection = Vector.build_identity_matrix ()};
	   }

let update sun_light dir =
	let dir = normalize dir in 
	sun_light.dir <- dir;
	let z_axis = scale dir (-1.0) in
	let rotation = build_identity_matrix () in
	ignore (set_z_axis rotation z_axis);

	let y_axis = [Vec3 (1.0,0.0,0.0); Vec3 (0.0,1.0,0.0)] |> List.map (fun v -> let z_axis = get_z_axis rotation in cross z_axis v) |> List.filter (fun v -> dot v v > 0.0) |> List.hd |> normalize in
	let x_axis = z_axis |> cross y_axis |> normalize in
	ignore (set_y_axis rotation y_axis);
	ignore (set_x_axis rotation x_axis);

	let grid_bbox = sun_light.grid_bbox in
	let translation = build_identity_matrix () in
	let Vec3 (x,y,z) = grid_bbox.Bounding_box.max in
	ignore (set_translation translation (Vec3 (0., 0., -1.0 *. z)));
	
	let Vec3 (x,y,z) = calc_dim grid_bbox in
	let zrange = z and w2 = x /. 2.0 and h2 = y /. 2.0 in
	let projection = build_ortho_proj (-1. *. w2) w2 h2 (-1. *. h2) 0.0 zrange in
	sun_light.grid_space.rotation <- rotation;
	sun_light.grid_space.translation <- translation; 
	sun_light.grid_space.projection <- projection;
	sun_light
;;


let create dir bbox =
	let sun_light = init in
	let dir = Vector.normalize dir in
	sun_light.dir <- dir;
	sun_light.grid_bbox <- bbox;
	update sun_light dir
;;
(*
	let z_axis = scale dir (-1.0) in
	let rotation = build_identity_matrix () in
	ignore (set_z_axis rotation z_axis);
	let y_axis = [Vec3 (1.0,0.0,0.0); Vec3 (0.0,1.0,0.0)] |> List.map (fun v -> let z_axis = get_z_axis rotation in cross z_axis v) |> List.filter (fun v -> dot v v > 0.0) |> List.hd |> normalize in
	let x_axis = z_axis |> cross y_axis |> normalize in
	ignore (set_y_axis rotation y_axis);
	ignore (set_x_axis rotation x_axis);

	let translation = build_identity_matrix () in
	let Vec3 (x,y,z) = grid_bbox.Bounding_box.max in
	ignore (set_translation translation (Vec3 (0., 0., -1.0 *. z)));
	
	let Vec3 (x,y,z) = calc_dim grid_bbox in
	let zrange = z and w2 = x /. 2.0 and h2 = y /. 2.0 in
	let projection = build_ortho_proj (-1. *. w2) w2 h2 (-1. *. h2) 0.0 zrange in
	(*print_matrix rotation;
	print_matrix translation;
	print_matrix projection;	*)
	{dir; grid_bbox; grid_space = {rotation; translation; projection}}
;;	*)
