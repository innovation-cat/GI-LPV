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

let indirect_light_on = ref false
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

let insert_textures () =
	let base = {Texture.tex_id = glGenTexture (); Texture.target = BindTex.GL_TEXTURE_2D ; Texture.name = "wall"} in
	let open Texture.Texture_Params_2D in
	let {width;height;bpp;pixels} = load_image "./brick1.tga" in
	let params = {init_param_2d with min_filter = GL.Min.GL_LINEAR; 
					 mag_filter = GL.Mag.GL_LINEAR;
		    		         source_format = if bpp=4 then GL.GL_RGBA else GL.GL_RGB;
			        	 internal_format = if bpp=4 then Glex.GL_RGBA else Glex.GL_RGB;
				         wrap_s = GL_REPEAT;
				         wrap_t = GL_REPEAT;
					 degree_of_anisotropy = 1}
	in
	Texture.create_texture_2d base params width height (Some pixels);
	let {width;height;bpp;pixels} = load_image "./stone08.tga" in
	let params = {params with source_format = if bpp=4 then GL.GL_RGBA else GL.GL_RGB;
				  internal_format = if bpp=4 then Glex.GL_RGBA else Glex.GL_RGB;}
	in 
	let base = {base with Texture.tex_id = glGenTexture (); Texture.name = "floor"} in
	Texture.create_texture_2d base params width height (Some pixels)

(*	Resource_Map.add "wall" (base,params) 
	glBindTexture ~target:BindTex.GL_TEXTURE_2D ~texture:tex_id;
	glTexImage2D ~target:TexTarget.GL_TEXTURE_2D ~level:0 ~internal_format:InternalFormat.GL_RGB ~width ~height ~format_:GL_RGB ~type_:GL_UNSIGNED_BYTE ~pixels;
	glTexParameter ~target:TexParam.GL_TEXTURE_2D ~param:(TexParam.GL_TEXTURE_MAG_FILTER Mag.GL_LINEAR);
	glTexParameter ~target:TexParam.GL_TEXTURE_2D ~param:(TexParam.GL_TEXTURE_MIN_FILTER Min.GL_LINEAR);*)
;;	
(*
let create_model () =
	let sc = open_in "../image/data.txt" in
	let vertexes = Model.setup sc in
	let bbox = Bounding_box.create () in
	let bbox = Array.fold_left (fun b {pos={x;y;z}; _ } -> Bounding_box.add_vertex (x,y,z) b) bbox vertexes in
	let buffer = { 	Model.vbo = glGenBuffer (); 
			Model.name = "test_model_vertex_buffer"; 
			Model.length = Array.length vertexes; 
			Model.element_size = 32; 
			Model.buffer_decl= [|{f_type=VEC3F; f_name="position"};{f_type=VEC3F; f_name="normal"}; {f_type=VEC2F; f_name="texcoord"}|];
		     	bbox
		     }
	in
	let buffer_size = buffer.Model.length * buffer.Model.element_size in
	let vertexes_array = Array.make buffer_size 0.0 in
	let index = ref 0 in
	Array.iter (fun {pos={x;y;z};nor={m;n;k};tc={s;t}} -> vertexes_array.(!index) <- x; vertexes_array.(!index+1) <- y;
							      vertexes_array.(!index+2) <- z; vertexes_array.(!index+3) <- m;
							      vertexes_array.(!index+4) <- n; vertexes_array.(!index+5) <- k;
							      vertexes_array.(!index+6) <- s; vertexes_array.(!index+7) <- t;
							      index := !index + 8) vertexes;
	let vertexes_bigarray = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout vertexes_array in
	glBindBuffer GL_ARRAY_BUFFER buffer.vbo;
	glBufferData GL_ARRAY_BUFFER (ba_sizeof vertexes_bigarray) vertexes_bigarray GL_STATIC_DRAW;
	insert_vertex_buffer buffer;
	close_in sc
;;

let create_rsm_shader () = 
	let vertex_shader = glCreateShader GL_VERTEX_SHADER in
	let sc = open_in "../shader/rsm.vp" in
	let size = in_channel_length sc in
	let vertex_shader_src = String.create size in
	ignore (input sc vertex_shader_src 0 size);
	glShaderSource vertex_shader vertex_shader_src;
	close_in sc;

	let fragment_shader = glCreateShader GL_FRAGMENT_SHADER in
	let sc = open_in "../shader/rsm.fp" in
	let size = in_channel_length sc in
	let fragment_shader_src = String.create size in
	ignore (input sc fragment_shader_src 0 size);
	glShaderSource fragment_shader fragment_shader_src;
	close_in sc;

	glCompileShader vertex_shader;
	glGetShaderCompileStatus_exn vertex_shader;
	glCompileShader fragment_shader;
	glGetShaderCompileStatus_exn fragment_shader;

	let program = glCreateProgram () in
	glAttachShader program vertex_shader;
	glAttachShader program fragment_shader;
	glLinkProgram program;
	let attributes = [|{Glsl_shader.name = "normal"; Glsl_shader.value = glGetAttribLocation program "normal"};
			   {Glsl_shader.name = "position"; Glsl_shader.value = glGetAttribLocation program "position"};
			   {Glsl_shader.name = "texcoord"; Glsl_shader.value = glGetAttribLocation program "texcoord"} |] 
	in
	let geometry_shader = glCreateShader GL_GEOMETRY_SHADER in
	let shader_info = {vertex_shader; fragment_shader; geometry_shader; program;attributes} in
	insert_shader_list "rsm" shader_info
;;

let create_final0_shader () = 
	let vertex_shader = glCreateShader GL_VERTEX_SHADER in
	let sc = open_in "../shader/final.vp" in
	let size = in_channel_length sc in
	let vertex_shader_src = String.create size in
	ignore (input sc vertex_shader_src 0 size);
	glShaderSource vertex_shader vertex_shader_src;
	close_in sc;

	let fragment_shader = glCreateShader GL_FRAGMENT_SHADER in
	let sc = open_in "../shader/final.fp" in
	let size = in_channel_length sc in
	let fragment_shader_src = String.create size in
	ignore (input sc fragment_shader_src 0 size);
	glShaderSource fragment_shader fragment_shader_src;
	close_in sc;

	glCompileShader vertex_shader;
	glGetShaderCompileStatus_exn vertex_shader;
	glCompileShader fragment_shader;
	glGetShaderCompileStatus_exn fragment_shader;

	let program = glCreateProgram () in
	glAttachShader program vertex_shader;
	glAttachShader program fragment_shader;
	glLinkProgram program;
	let attributes = [|{Glsl_shader.name = "normal"; Glsl_shader.value = glGetAttribLocation program "normal"};
			   {Glsl_shader.name = "position"; Glsl_shader.value = glGetAttribLocation program "position"};
			   {Glsl_shader.name = "texcoord"; Glsl_shader.value = glGetAttribLocation program "texcoord"} |] 
	in
	let geometry_shader = glCreateShader GL_GEOMETRY_SHADER in
	let shader_info = {vertex_shader; fragment_shader; geometry_shader; program;attributes} in
	insert_shader_list "final0" shader_info
;;

let create_final1_shader () = 
	let vertex_shader = glCreateShader GL_VERTEX_SHADER in
	let sc = open_in "../shader/final.vp" in
	let size = in_channel_length sc in
	let vertex_shader_src = String.create size in
	ignore (input sc vertex_shader_src 0 size);
	let sources = [|"#define NO_INDIRECT_LIGHT"; vertex_shader_src|] in
	glShaderSources vertex_shader 2 (Some sources) None;
	close_in sc;

	let fragment_shader = glCreateShader GL_FRAGMENT_SHADER in
	let sc = open_in "../shader/final.fp" in
	let size = in_channel_length sc in
	let fragment_shader_src = String.create size in
	ignore (input sc fragment_shader_src 0 size);
	let sources = [|"#define NO_INDIRECT_LIGHT"; fragment_shader_src|] in
	glShaderSources fragment_shader 2 (Some sources) None;
	close_in sc;

	glCompileShader vertex_shader;
	glGetShaderCompileStatus_exn vertex_shader; 
	glCompileShader fragment_shader;
	glGetShaderCompileStatus_exn fragment_shader; 

	let program = glCreateProgram () in
	glAttachShader program vertex_shader;
	glAttachShader program fragment_shader;
	glLinkProgram program;
	let attributes = [|{Glsl_shader.name = "normal"; Glsl_shader.value = glGetAttribLocation program "normal"};
			   {Glsl_shader.name = "position"; Glsl_shader.value = glGetAttribLocation program "position"};
			   {Glsl_shader.name = "texcoord"; Glsl_shader.value = glGetAttribLocation program "texcoord"} |] 
	in
	
	let geometry_shader = glCreateShader GL_GEOMETRY_SHADER in
	let shader_info = {vertex_shader; fragment_shader; geometry_shader; program;attributes} in
	insert_shader_list "final1" shader_info
;;
*)
let create_grid_bounding_box () =
	match !test_model with
		   None -> raise (Failure "no test model.")
		|  Some tm -> let v = Bounding_box.calc_dim tm.Model.bbox in
				Vector.print v;
				let d = (Vector.length v) *. 0.5 in
				grid_bbox := tm.Model.bbox;
				Bounding_box.add_vertex (d, d, d) !grid_bbox;
				Bounding_box.add_vertex (-1. *. d, -1. *. d, -1. *. d) !grid_bbox
;;
(*
let create_testmodel_shader () = 
	create_rsm_shader ();
	create_final0_shader ();
	create_final1_shader ()
;;
*)	
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
(*
  	gluPerspective 45.0 ((float width) /. (float height)) 0.1 100.0;  (* Calculate The Aspect Ratio Of The Window *)
*)
  	glMatrixMode GL_MODELVIEW;
	glLoadIdentity();

	insert_textures ();
	test_model := (Some (Model.create "../image/data.txt"));
	(*create_testmodel_shader ();*)
	create_grid_bounding_box ();
	Vector.print (!grid_bbox).Bounding_box.min;
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

	Printf.printf "all end\n";
	(!texture_list) |> Texture.Resource_Map.bindings |> List.iter (fun (a,b) -> Printf.printf "%s\n" a);
	(!shader_list) |> Glsl_shader.Resource_Map.bindings |> List.iter (fun (a,b) -> Printf.printf "%s\n" a);
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
	     |  (Some test_model, Some sun_light, Some grid, Some rsm, Some depth_normal_buffer, Some indirect_light_buffer) 
		-> Model.draw_to_rsm test_model sun_light rsm;
		   let view_mat = Camera.get_transform cam (float (!frame_duration)) in
		   calc_proj_mat ();

		   let Vector.Vec3 (dir_x, dir_y, dir_z) = sun_light.Directional_light.dir in
		   let sky_color = (-1.) *. dir_y |> max 0.0 |> min 1.0 in
  		   Printf.printf "%f\n" sky_color;
		   glClearColor 0.0 0.0 sky_color 0.0;
		   glClear [GL_DEPTH_BUFFER_BIT; GL_COLOR_BUFFER_BIT];   (* Clear The Screen And The Depth Buffer *)
		   glViewport 0 0 !win_width !win_height;
		   Model.draw test_model view_mat !proj_mat indirect_light_buffer sun_light rsm (!indirect_light_on);
		   glEnable GL.GL_CULL_FACE;
		   glPolygonMode GL.GL_FRONT_AND_BACK GL.GL_FILL;
		   glUnuseProgram (); 
		   glutSwapBuffers ()

with
	_ -> raise (Failure "draw nodel fail.")

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
		
		light_rotation := !light_rotation +. pi *. !rotate_dir *. (float !frame_duration) /. 10000.0;
		Printf.printf "%f\n" !light_rotation;
		Vector.print light_dir;
		Vector.print_matrix rot;	
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

