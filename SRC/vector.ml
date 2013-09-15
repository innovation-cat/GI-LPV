type vec =   Vec2 of float * float 
	   | Vec3 of float * float * float
	   | Vec4 of float * float * float * float

let add vec1 vec2 = 
	match (vec1,vec2) with
	    (Vec2 (x1,y1), Vec2 (x2,y2)) -> Vec2 (x1 +. x2, y1 +. y2)
	|   (Vec3 (x1,y1,z1), Vec3 (x2,y2,z2)) -> Vec3 (x1 +. x2, y1 +. y2, z1 +. z2)
	|   (Vec4 (x1,y1,z1,r1), Vec4 (x2,y2,z2,r2)) -> Vec4 (x1 +. x2, y1 +. y2, z1 +. z2, r1 +. r2)
	|   _ -> raise (Failure "uncompatible vector operation.")

let (++) vec1 vec2 = add vec1 vec2

 
let sub vec1 vec2 = 
	match (vec1,vec2) with
	    (Vec2 (x1,y1), Vec2 (x2,y2)) -> Vec2 (x1 -. x2, y1 -. y2)
	|   (Vec3 (x1,y1,z1), Vec3 (x2,y2,z2)) -> Vec3 (x1 -. x2, y1 -. y2, z1 -. z2)
	|   (Vec4 (x1,y1,z1,r1), Vec4 (x2,y2,z2,r2)) -> Vec4 (x1 -. x2, y1 -. y2, z1 -. z2, r1 -. r2)
	|   _ -> raise (Failure "uncompatible vector operation.")

let (--) vec1 vec2 = sub vec1 vec2

let scale vec s =
	match vec with 
	    Vec2 (x1,y1) -> Vec2 (x1*.s, y1*.s)
	|   Vec3 (x1,y1,z1) -> Vec3 (x1*.s, y1*.s, z1*.s)
	|   Vec4 (x1,y1,z1,r1) -> Vec4 (x1*.s, y1*.s, z1*.s, r1*.s)

let dot vec1 vec2 = 
	match (vec1,vec2) with
	    (Vec2 (x1,y1), Vec2 (x2,y2)) -> x1*.x2 +. y1*.y2 
	|   (Vec3 (x1,y1,z1), Vec3 (x2,y2,z2)) -> x1*.x2 +. y1*.y2 +. z1*.z2
	|   (Vec4 (x1,y1,z1,r1), Vec4 (x2,y2,z2,r2)) -> x1*.x2 +. y1*.y2 +. z1*.z2 +. r1*.r2
	|   _ -> raise (Failure "uncompatible vector operation.")


let length vec = sqrt (dot vec vec)

let normalize vec = let d = length vec in if d=0.0 then raise (Failure "zero.") else scale vec (1.0/.d)

(* we only support 3 dimension vector *)
let cross vec1 vec2 =
	match (vec1, vec2) with
	    (Vec3 (x1,y1,z1), Vec3 (x2,y2,z2)) -> Vec3 (y1*.z2 -. y2*.z1 , x2*.z1 -. x1*.z2, x1*.y2 -. x2*.y1)
	|   _ -> raise (Failure "uncompatible vector operation.")

let print = function 
	   Vec3 (x,y,z) -> Printf.printf "(%f , %f , %f)\n" x y z
	|  Vec2 (x,y)   -> Printf.printf "(%f , %f)\n" x y
	|  Vec4 (x,y,z,r) -> Printf.printf "(%f , %f , %f , %f)\n" x y z r

(**************************************************************************************************************************)

(* type matrix                  *)
(* [0  4  8  12]                *)
(* [1  5  9  13]                *)
(* [2  6  10 14]                *)
(* [3  7  11 15]                *)

let build_identity_matrix () = [|1.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 1.0|];;

let build_scale_matrix x y z = [|x; 0.0; 0.0; 0.0; 0.0; y; 0.0; 0.0; 0.0; 0.0; z; 0.0; 0.0; 0.0; 0.0; 1.0|];;

let build_translation_matrix x y z = [|1.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; x; y; z; 1.0|];;

let get_z_axis m = Vec3 (m.(2), m.(6), m.(10))
let get_y_axis m = Vec3 (m.(1), m.(5), m.(9))
let get_x_axis m = Vec3 (m.(0), m.(4), m.(8))
let get_translation m = Vec3 (m.(12), m.(13), m.(14))

let set_z_axis m v = let Vec3 (x,y,z) = v in m.(2) <- x; m.(6) <- y; m.(10) <- z; m
let set_y_axis m v = let Vec3 (x,y,z) = v in m.(1) <- x; m.(5) <- y; m.(9) <- z; m
let set_x_axis m v = let Vec3 (x,y,z) = v in m.(0) <- x; m.(4) <- y; m.(8) <- z; m
let set_translation m v = let Vec3 (x,y,z) = v in m.(12) <- x; m.(13) <- y; m.(14) <- z; m

let transform m = function
	      Vec3 (x,y,z) as v -> Vec3 ( (dot (get_x_axis m) v) +. m.(12),
				   	  (dot (get_y_axis m) v) +. m.(13),
					  (dot (get_z_axis m) v) +. m.(14) )
	  |   Vec4 (x,y,z,r) as v -> Vec4 ( dot (Vec4 (m.(0), m.(4), m.(8), m.(12))) v, 
					    dot (Vec4 (m.(1), m.(5), m.(9), m.(13))) v, 
					    dot (Vec4 (m.(2), m.(6), m.(10), m.(14))) v, 
					    dot (Vec4 (m.(3), m.(7), m.(11), m.(15))) v)
	  |   _ -> raise (Failure "unexpected error.")

(* m1 * m2 *)
let mult m1 m2 = 
	[| m1.(0) *. m2.(0) +. m1.(4) *. m2.(1) +. m1.(8) *. m2.(2) +. m1.(12) *. m2.(3);
	   m1.(1) *. m2.(0) +. m1.(5) *. m2.(1) +. m1.(9) *. m2.(2) +. m1.(13) *. m2.(3);
	   m1.(2) *. m2.(0) +. m1.(6) *. m2.(1) +. m1.(10) *. m2.(2) +. m1.(14) *. m2.(3);
	   m1.(3) *. m2.(0) +. m1.(7) *. m2.(1) +. m1.(11) *. m2.(2) +. m1.(15) *. m2.(3);
	   m1.(0) *. m2.(4) +. m1.(4) *. m2.(5) +. m1.(8) *. m2.(6) +. m1.(12) *. m2.(7);
	   m1.(1) *. m2.(4) +. m1.(5) *. m2.(5) +. m1.(9) *. m2.(6) +. m1.(13) *. m2.(7);
	   m1.(2) *. m2.(4) +. m1.(6) *. m2.(5) +. m1.(10) *. m2.(6) +. m1.(14) *. m2.(7);
	   m1.(3) *. m2.(4) +. m1.(7) *. m2.(5) +. m1.(11) *. m2.(6) +. m1.(15) *. m2.(7);
	   m1.(0) *. m2.(8) +. m1.(4) *. m2.(9) +. m1.(8) *. m2.(10) +. m1.(12) *. m2.(11);
	   m1.(1) *. m2.(8) +. m1.(5) *. m2.(9) +. m1.(9) *. m2.(10) +. m1.(13) *. m2.(11);
	   m1.(2) *. m2.(8) +. m1.(6) *. m2.(9) +. m1.(10) *. m2.(10) +. m1.(14) *. m2.(11);
	   m1.(3) *. m2.(8) +. m1.(7) *. m2.(9) +. m1.(11) *. m2.(10) +. m1.(15) *. m2.(11);
	   m1.(0) *. m2.(12) +. m1.(4) *. m2.(13) +. m1.(8) *. m2.(14) +. m1.(12) *. m2.(15);
	   m1.(1) *. m2.(12) +. m1.(5) *. m2.(13) +. m1.(9) *. m2.(14) +. m1.(13) *. m2.(15);
	   m1.(2) *. m2.(12) +. m1.(6) *. m2.(13) +. m1.(10) *. m2.(14) +. m1.(14) *. m2.(15);
	   m1.(3) *. m2.(12) +. m1.(7) *. m2.(13) +. m1.(11) *. m2.(14) +. m1.(15) *. m2.(15)|]

let pi_over_360 = 1.0/.360.0;;

let build_persp_proj fov aspect znear zfar = 
	let h = 1. /. tan (fov *. pi_over_360) in	
        let neg_depth = znear -. zfar in
	[| h /. aspect; 0.0; 0.0; 0.0;
	   0.0; h; 0.0; 0.0; 
	   0.0; 0.0; (zfar +. znear) /. neg_depth; -1.0; 
	   0.0; 0.0; 2.0 *. (znear *. zfar) /. neg_depth; 0.0 |]


let build_ortho_proj left right top bottom znear zfar = 
	let width = right -. left in
	let height = top -. bottom in
	let dz = zfar -. znear in
	[| 2.0 /. width; 0.; 0.; 0.; 
	   0.; 2.0 /. height; 0.; 0.; 
	   0.; 0.; (-2.0) /. dz; 0.; 
	   ((-1. *. right) -. left) /. width; ((-1. *. top) -. bottom) /. height; ((-1. *. zfar) -. znear) /. dz; 1.0 |]


let transpose m = [|m.(0); m.(4); m.(8); m.(12); m.(1);m.(5); m.(9); m.(13); m.(2); m.(6); m.(10); m.(14); m.(3); m.(7); m.(11); m.(15)|] 

let build_rotation_mat_1  pitch yaw roll =
	let cx = cos pitch in
	let sx = sin pitch in
	let cy = cos yaw in
	let sy = sin yaw in
	let cz = cos roll in
	let sz = sin roll in

	let cxsy = cx *. sy in
	let sxsy = sx *. sy in
	transpose [| cy *. cz; -1. *. cy *. sz; -1. *. sy; 0.0; -1. *. sxsy *. cz +. cx *. sz; sxsy *. sz +. cx *. cz; -1. *. sx *. cy; 0.0;
	   	     cxsy *. cz +. sx *. sz; -1. *. cxsy *. sz +. sx *. cz; cx *. cy; 0.0; 0.0; 0.0; 0.0; 1.0|]

let build_rotation_mat_2 theta r = 
	let Vec3 (x,y,z) = r in
	let m = build_identity_matrix () in
	let costheta = cos theta in
	let sintheta = sin theta in

	let m = set_x_axis m (Vec3 (costheta +. (1.0 -. costheta) *. x *. x, 
			  (1.0 -. costheta) *. x *. y -. z *. sintheta, 
			  (1.0 -. costheta) *. x *. z +. y *. sintheta)) in

	let m = set_y_axis m (Vec3 ( (1.0 -. costheta) *. x *. y +. z *. sintheta,
			     costheta +. (1.0 -. costheta) *. y *. y, 
			     (1.0 -. costheta) *. y *. z -. x *. sintheta)) in

	set_z_axis m (Vec3 ( (1.0 -. costheta) *. x *. z -. y *. sintheta, 
			     (1.0 -. costheta) *. y *. z +. x *. sintheta, 
			      costheta +. (1.0 -. costheta) *. z *. z ))



let inverse m =
	let det = m.(0) *. m.(5) *. m.(10) +. m.(4) *. m.(9) *. m.(2) +. m.(8) *. m.(1) *. m.(6) -. m.(8) *. m.(5) *. m.(2) -. m.(4) *. m.(1) *. m.(10) -. m.(0) *. m.(9) *. m.(6) in
	let det = 1.0 /. det in
	[| (m.(5) *. m.(10) -. m.(9) *. m.(6)) *. det; -1. *. (m.(1) *. m.(10) -. m.(9) *. m.(2)) *. det;
	   (m.(1) *. m.(6) -. m.(5) *. m.(2)) *. det;  0.0; 
	   -1. *. (m.(4) *. m.(10) -. m.(8) *. m.(6)) *. det; (m.(0) *. m.(10) -. m.(8) *. m.(2)) *. det;
	   -1. *. (m.(0) *. m.(6) -. m.(4) *. m.(2)) *. det; 0.0;
	   (m.(4) *. m.(9) -. m.(8) *. m.(5)) *. det; -1. *. (m.(0) *. m.(9) -. m.(8) *. m.(1)) *. det;
	   (m.(0) *. m.(5) -. m.(4) *. m.(1)) *. det; 0.0;
	   -1. *. (m.(12) *. m.(0) +. m.(13) *. m.(4) +. m.(14) *. m.(8)); -1. *. (m.(12) *. m.(1) +. m.(13) *. m.(5) +. m.(14) *. m.(9));
	   -1. *. (m.(12) *. m.(2) +. m.(13) *. m.(6) +. m.(14) *. m.(10)); 1.0 |]

let set_x_rotation rad = 
	let m = build_identity_matrix () in
	let c = cos rad in
	let s = sin rad in
	m.(5) <- c; m.(6) <- s;	m.(9) <- -1. *. s; m.(10) <- c;	
	m

let set_y_rotation rad = 
	let m = build_identity_matrix () in
	let c = cos rad in
	let s = sin rad in
	m.(0) <- c; m.(2) <- -1. *. s; m.(8) <- s; m.(10) <- c;	
	m

let set_z_rotation rad =  
	let m = build_identity_matrix () in
	let c = cos rad in
	let s = sin rad in
	m.(0) <- c; m.(1) <- s;	m.(4) <- -1. *. s; m.(5) <-  c;	
	m

let print_matrix m = 
	Printf.printf "%f %f %f %f\n" m.(0) m.(4) m.(8) m.(12);
	Printf.printf "%f %f %f %f\n" m.(1) m.(5) m.(9) m.(13);
	Printf.printf "%f %f %f %f\n" m.(2) m.(6) m.(10) m.(14);
	Printf.printf "%f %f %f %f\n" m.(3) m.(7) m.(11) m.(15)
