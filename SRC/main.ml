open GL       (* Module For The OpenGL Library *)
open Glu      (* Module For The GLu Library *)
open Glut     (* Module For The GLUT Library *)
open VBO
open Glex
open Texture
open Loadtga
open Model
open Vector
open Glsl_shader
open Directional_light

(* rotation angle for the triangle. *)
let pi = 4.0 *. (atan 1.0)

let rtri = ref 0.0

(* rotation angle for the quadrilateral. *)
let rquad = ref 0.0

let win_width = ref 1280 and win_height = ref 720
let znear = ref 1.0 and zfar = ref 200.0

let (|>) x f = f x

let grid_bbox = ref (Bounding_box.init) 
let cam = Camera.init_camera

let test_model = ref None
let rsm = ref None
let sun_light = ref None

let proj_mat_dirty = ref true
let proj_mat = ref (Vector.build_identity_matrix ())

let indirect_light_on = ref true
let rotate_light = ref true (* indicate whether rotate light *)
let light_rotation = ref (pi/.2.0) (* rotation angle *)
let rotate_dir = ref (-1.0) (* only 1.0 or -1.0 *)


(* light_rotation_axis_1:                                  *)
(* 	true : sun_light.dir = Vector.Vec3 (1.0, 0.0, 0.0) *)
(* 	false: sun_light.dir = Vector.Vec3 (1.0, 0.0, 1.0) *)
let light_rotation_axis_1 = ref true

let depth_normal_buffer = ref None
let indirect_light_buffer = ref None
let grid = ref None

let this_frame = ref 0 and prev_frame = ref 0 and frame_duration = ref 0


let calc_proj_mat () =
	if !proj_mat_dirty = true then begin
		let fov = 60. in
		let aspect = (float !win_width) /. (float !win_height) in
		proj_mat := Vector.build_persp_proj fov aspect !znear !zfar;
		proj_mat_dirty := false
	end
;;

(* create wall and flloor texture *)
let insert_textures () =
	let base = {Texture.tex_id = glGenTexture (); Texture.target = BindTex.GL_TEXTURE_2D ; Texture.name = "wall"} in
	let open Texture.Texture_Params_2D in
	let {width;height;bpp;pixels} = load_image "./brick1.tga" in
	let params = {init_param_2d with min_filter = GL.Min.GL_LINEAR_MIPMAP_LINEAR; 
					 mag_filter = GL.Mag.GL_LINEAR;
				         wrap_s = GL_REPEAT;
				         wrap_t = GL_REPEAT;
					 degree_of_anisotropy = 16}
	in
	Texture.create_texture_2d base params width height (Some pixels);

	let {width;height;bpp;pixels} = load_image "./stone08.tga" in
	let params = {params with source_format = if bpp=4 then GL.GL_RGBA else GL.GL_RGB;
				  internal_format = if bpp=4 then Glex.GL_RGBA else Glex.GL_RGB;}
	in 
	let base = {base with Texture.tex_id = glGenTexture (); Texture.name = "floor"} in
	Texture.create_texture_2d base params width height (Some pixels)
;;	


(* create bounding box of grid, this is not the same as bounding box of test_model *)
let create_grid_bounding_box () =
	match !test_model with
		   None -> raise (Failure "grid bounding_box creation failure, no test model.")
		|  Some tm -> let v = Bounding_box.calc_dim tm.Model.bbox in
				let d = (Vector.length v) *. 0.5 in
				grid_bbox := tm.Model.bbox;
				Bounding_box.add_vertex (d, d, d) !grid_bbox;
				Bounding_box.add_vertex (-1. *. d, -1. *. d, -1. *. d) !grid_bbox
;;


(* A general OpenGL initialization function.  Sets all of the initial parameters. *)
let initGL ~width ~height =                     (* We call this right after our OpenGL window is created. *)
	glClearColor 0.0 0.0 0.0 0.0;                 (* This Will Clear The Background Color To Black *)
	glClearDepth 1.0;                             (* Enables Clearing Of The Depth Buffer *)
  	glEnable GL.GL_CULL_FACE;
	glCullFace GL.GL_BACK;
  	glShadeModel GL_SMOOTH;                       (* Enables Smooth Color Shading *)
	glDisable GL.GL_LIGHTING;
  	glEnable GL_DEPTH_TEST;                       (* Enables Depth Testing *)
	glDepthFunc GL_LESS;                          (* The Type Of Depth Test To Do *)

  	glMatrixMode GL_PROJECTION;
  	glLoadIdentity();                             (* Reset The Projection Matrix *)
  	glMatrixMode GL_MODELVIEW;
	glLoadIdentity();

	insert_textures ();
	
	test_model := (Some (Model.create "../image/data.txt"));

	create_grid_bounding_box ();
	
	sun_light := Some (Directional_light.create (Vec3 (0., -1., -1.)) (!grid_bbox));
	let rsm_width = 512 and rsm_height = 512 in
	rsm := Some (Rsm.create rsm_width rsm_height);
	depth_normal_buffer := Some (Depth_normal_buffer.create !win_width !win_height !znear !zfar 512);
	indirect_light_buffer := Some (Indirect_light_buffer.create (!win_width/2) (!win_height/2));
	(*Grid.create_shader ();*)

	begin 
		match !sun_light with
			  None -> raise (Failure "no sun light.")
			| Some sl -> grid := Some (Grid.create (!grid_bbox) 16 16 16 rsm_width rsm_height sl)
	end;

	(!texture_list) |> Texture.Resource_Map.bindings |> List.iter (fun (a,b) -> Printf.printf "%s\n" a; Texture.print_texture_info b);
	Printf.printf "\n\n";
	(!shader_list) |> Glsl_shader.Resource_Map.bindings |> List.iter (fun (a,b) -> Printf.printf "%s\n" a);
	Printf.printf "\n\n";
	(!GL_buffer.buffer_list) |> GL_buffer.Resource_Map.bindings |> List.iter (fun (a,b) -> Printf.printf "%s\n" a);
	flush stdout
;;

(* The function called when our window is resized (which shouldn't happen, because we're fullscreen) *)
let reSizeGLScene ~width ~height =
	win_width := width;
	win_height := height;
	proj_mat_dirty := true; 
(*	let height =
    		if height=0 then 1                          (* Prevent A Divide By Zero If The Window Is Too Small *)
    		else height
  	in

  	glViewport 0 0 width height;                  (* Reset The Current Viewport And Perspective Transformation *)

  	glMatrixMode GL_PROJECTION;
  	glLoadIdentity();

  	gluPerspective 45.0 ((float width) /. (float height)) 0.1 10000.0;
  	glMatrixMode GL_MODELVIEW;
*)
;;


(* The main drawing function. *)
let drawGLScene() =

try
	match (!test_model, !sun_light, !grid, !rsm, !depth_normal_buffer, !indirect_light_buffer) with
		(None,_,_,_,_,_) | (_,None,_,_,_,_) | (_,_,None,_,_,_) | (_,_,_,None,_,_) | (_,_,_,_,None,_) | (_,_,_,_,_,None) 
		-> ()
	     |  (Some _test_model, Some _sun_light, Some _grid, Some _rsm, Some _depth_normal_buffer, Some _indirect_light_buffer) 
		-> Model.draw_to_rsm _test_model _sun_light _rsm;
		   let view_mat = Camera.get_transform cam (float (!frame_duration)) in
		   calc_proj_mat ();

		   let (_test_model, _sun_light, _grid, _rsm, _depth_normal_buffer, _indirect_light_buffer) = 
		   if !indirect_light_on = true then begin
			let _depth_normal_buffer = Depth_normal_buffer.set_begin _depth_normal_buffer view_mat !proj_mat _sun_light in
			Depth_normal_buffer.draw _depth_normal_buffer _test_model;
			Depth_normal_buffer.set_end _depth_normal_buffer;
			
			Grid.inject_from_depth_normal_buffer _grid _depth_normal_buffer;
			Grid.inject_from_rsm _grid _rsm;
			Grid.select_grid _grid;
			
			let gv_texture = _grid.Grid.gv2.Geometry_volume.tex in
			let _grid = Grid.inject_vpls _grid _rsm gv_texture in
			Indirect_light_buffer.set_begin _indirect_light_buffer view_mat !proj_mat _sun_light (Vector.Vec3 (16.0, 16.0, 16.0));
			Grid.bind_light_volume_textures _grid;
			Indirect_light_buffer.draw _indirect_light_buffer _test_model;

			Grid.unbind_light_volume_textures _grid;
			Indirect_light_buffer.set_end _indirect_light_buffer;
	
			Indirect_light_buffer.blur _indirect_light_buffer _depth_normal_buffer (Vector.Vec3 (16.0, 16.0, 16.0)) _sun_light;
			(_test_model, _sun_light, _grid, _rsm, _depth_normal_buffer, _indirect_light_buffer)
		   end
		   else
			(_test_model, _sun_light, _grid, _rsm, _depth_normal_buffer, _indirect_light_buffer)
		   in

		   let Vector.Vec3 (dir_x, dir_y, dir_z) = _sun_light.Directional_light.dir in
		   let sky_color = (-1.) *. dir_y |> max 0.0 |> min 1.0 in
		   glClearColor 0.0 0.0 sky_color 0.0;
		   glClear [GL_DEPTH_BUFFER_BIT; GL_COLOR_BUFFER_BIT];   (* Clear The Screen And The Depth Buffer *)
		   glViewport 0 0 !win_width !win_height;

		   Model.draw _test_model view_mat !proj_mat _indirect_light_buffer _sun_light _rsm (!indirect_light_on);

		   glEnable GL.GL_CULL_FACE;
		   glPolygonMode GL.GL_FRONT_AND_BACK GL.GL_FILL;
		   
		   (* update *)
		   test_model := Some _test_model; 
		   sun_light := Some _sun_light; 
		   grid := Some _grid;
		   rsm := Some _rsm; 
		   depth_normal_buffer := Some _depth_normal_buffer; 
		   indirect_light_buffer := Some _indirect_light_buffer;
		   
		   glUnuseProgram (); 
		   glutSwapBuffers ()

with
	Failure str -> Printf.printf "%s\n" str

;;


(* The function called whenever a key is pressed. *)
let keyPressed ~window ~key ~x ~y =

  	(* If escape is pressed, kill everything. *)
  	if key = '\027' then begin
    (* shut down our window *)
    		glutDestroyWindow window;
    (* exit the program...normal termination. *)
    		exit 0;
  	end;
;;

let idlefunc () =
	if !this_frame = 0 then begin
		prev_frame := Glut.glutGet(Glut.GLUT_ELAPSED_TIME);
		this_frame := !prev_frame
	end
	else begin
		prev_frame := !this_frame;
		this_frame := Glut.glutGet(Glut.GLUT_ELAPSED_TIME)
	end;
	
	frame_duration := !this_frame - !prev_frame;

	Printf.printf "frame_duration: %d\n" (!frame_duration);
	
	if !rotate_light = true then begin
		if !light_rotation > pi then begin
			rotate_dir := !rotate_dir *. (-1.0);
			light_rotation := pi;
		end
		else if !light_rotation < 0.0 then begin
			rotate_dir := !rotate_dir *. (-1.0);
			light_rotation := 0.0;
		end;
		
		let rot = if !light_rotation_axis_1 = true then 
				Vector.build_rotation_mat_2 ( (-1.) *. !light_rotation) (Vector.Vec3 (1.0, 0.0, 0.0))
			 else 
				let axis = Vector.normalize (Vector.Vec3 (1.0, 0.0, 1.0)) in
				Vector.build_rotation_mat_2 ( (-1.) *. !light_rotation) axis
		in
		let light_dir = if !light_rotation_axis_1 = true then
					Vector.transform rot (Vector.Vec3 (0.0, 0.0, -1.0)) 
				else 
					let axis = Vector.normalize (Vector.Vec3 (1.0, 0.0, -1.0)) in
					Vector.transform rot axis
		in
		
		light_rotation := !light_rotation +. pi *. !rotate_dir *. (float !frame_duration) /. 30000.0;
		begin 
			match !sun_light with
				  None -> ()
				| Some sun -> sun_light := Some (Directional_light.update sun light_dir)
		end;
	end;
	glutPostRedisplay ();
;;

let () =
  	ignore(glutInit Sys.argv);

  	glutInitDisplayMode [GLUT_RGBA; GLUT_DOUBLE; GLUT_ALPHA; GLUT_DEPTH];

  	glutInitWindowSize !win_width !win_height;

  	glutInitWindowPosition 0 0;

  	let window = glutCreateWindow "Light Propagation Volume" in

  	glutDisplayFunc drawGLScene;

 	(*glutFullScreen();*)

  	glutIdleFunc idlefunc;

  	glutReshapeFunc reSizeGLScene;

  	glutKeyboardFunc (keyPressed ~window);

  	initGL 640 480;

  	glutMainLoop();
;;

