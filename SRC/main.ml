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

(* rotation angle for the triangle. *)
let rtri = ref 0.0

(* rotation angle for the quadrilateral. *)
let rquad = ref 0.0

let (|>) x f = f x

let insert_textures () =
	Printf.printf "begin\n" ;
	let base = {Texture.tex_id = glGenTexture (); Texture.target = BindTex.GL_TEXTURE_2D ; Texture.name = "wall"} in
	let open Texture.Texture_Params_2D in
	let {width;height;bpp;pixels} = load_image "./brick1.tga" in
	let params = {init_param_2d with min_filter = GL.Min.GL_LINEAR; 
					 mag_filter = GL.Mag.GL_LINEAR;
		    		         source_format = if bpp=4 then GL_RGBA else GL_RGB;
			        	 internal_format = if bpp=4 then InternalFormat.GL_RGBA else InternalFormat.GL_RGB;
				         wrap_s = GL_REPEAT;
				         wrap_t = GL_REPEAT;
					 degree_of_anisotropy = 1}
	in 
	Texture.create_texture_2d ~base ~params ~width ~height ~pixels;
	let {width;height;bpp;pixels} = load_image "./stone08.tga" in
	let params = {params with source_format = if bpp=4 then GL_RGBA else GL_RGB;
				  internal_format = if bpp=4 then InternalFormat.GL_RGBA else InternalFormat.GL_RGB;}
	in 
	let base = {base with Texture.tex_id = glGenTexture (); Texture.name = "floor"} in
	Texture.create_texture_2d ~base ~params ~width ~height ~pixels

(*	Resource_Map.add "wall" (base,params) 
	glBindTexture ~target:BindTex.GL_TEXTURE_2D ~texture:tex_id;
	glTexImage2D ~target:TexTarget.GL_TEXTURE_2D ~level:0 ~internal_format:InternalFormat.GL_RGB ~width ~height ~format_:GL_RGB ~type_:GL_UNSIGNED_BYTE ~pixels;
	glTexParameter ~target:TexParam.GL_TEXTURE_2D ~param:(TexParam.GL_TEXTURE_MAG_FILTER Mag.GL_LINEAR);
	glTexParameter ~target:TexParam.GL_TEXTURE_2D ~param:(TexParam.GL_TEXTURE_MIN_FILTER Min.GL_LINEAR);*)
;;	

let create_model () =
	let vertexes = Model.setup (open_in "../image/data.txt") in
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
	insert_vertex_buffer buffer 
;;

let create_rsm_shader () = 
	let vertex_shader = glCreateShader GL_VERTEX_SHADER in
	let sc = open_in "../shader/rsm.vp" in
	let size = in_channel_length sc in
	let vertex_shader_src = String.create size in
	ignore (input sc vertex_shader_src 0 size);
	glShaderSource vertex_shader vertex_shader_src;

	let fragment_shader = glCreateShader GL_FRAGMENT_SHADER in
	let sc = open_in "../shader/rsm.fp" in
	let size = in_channel_length sc in
	let fragment_shader_src = String.create size in
	ignore (input sc fragment_shader_src 0 size);
	glShaderSource fragment_shader fragment_shader_src;

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

	let fragment_shader = glCreateShader GL_FRAGMENT_SHADER in
	let sc = open_in "../shader/final.fp" in
	let size = in_channel_length sc in
	let fragment_shader_src = String.create size in
	ignore (input sc fragment_shader_src 0 size);
	glShaderSource fragment_shader fragment_shader_src;

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

	let fragment_shader = glCreateShader GL_FRAGMENT_SHADER in
	let sc = open_in "../shader/final.fp" in
	let size = in_channel_length sc in
	let fragment_shader_src = String.create size in
	ignore (input sc fragment_shader_src 0 size);
	let sources = [|"#define NO_INDIRECT_LIGHT"; fragment_shader_src|] in
	glShaderSources fragment_shader 2 (Some sources) None;

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

let create_shader () = 
	create_rsm_shader ();
	create_final0_shader ();
	create_final1_shader ();
;;
	
(* A general OpenGL initialization function.  Sets all of the initial parameters. *)
let initGL ~width ~height =                     (* We call this right after our OpenGL window is created. *)
  	glClearColor 0.0 0.0 0.0 0.0;                 (* This Will Clear The Background Color To Black *)
	glClearDepth 1.0;                             (* Enables Clearing Of The Depth Buffer *)
  	glDepthFunc GL_LESS;                          (* The Type Of Depth Test To Do *)
  	glEnable GL_DEPTH_TEST;                       (* Enables Depth Testing *)
	glEnable GL_TEXTURE_2D;
  	glShadeModel GL_SMOOTH;                       (* Enables Smooth Color Shading *)

  	glMatrixMode GL_PROJECTION;
  	glLoadIdentity();                             (* Reset The Projection Matrix *)

  	gluPerspective 45.0 ((float width) /. (float height)) 0.1 100.0;  (* Calculate The Aspect Ratio Of The Window *)

  	glMatrixMode GL_MODELVIEW;
	Printf.printf "init begin.";	
	insert_textures ();
	create_model ();
	create_shader ()


(* The function called when our window is resized (which shouldn't happen, because we're fullscreen) *)
let reSizeGLScene ~width ~height =
  let height =
    if height=0 then 1                          (* Prevent A Divide By Zero If The Window Is Too Small *)
    else height
  in

  glViewport 0 0 width height;                  (* Reset The Current Viewport And Perspective Transformation *)

  glMatrixMode GL_PROJECTION;
  glLoadIdentity();

  gluPerspective 45.0 ((float width) /. (float height)) 0.1 10000.0;
  glMatrixMode GL_MODELVIEW;
;;


(* The main drawing function. *)
let drawGLScene() =

  glClear [GL_DEPTH_BUFFER_BIT; GL_COLOR_BUFFER_BIT];   (* Clear The Screen And The Depth Buffer *)
  glLoadIdentity();                             (* Reset The View *)

  glTranslate (-1.5) (0.0) (-6.0);              (* Move Left 1.5 Units And Into The Screen 6.0 *)

	glRotate (!rquad) 1.0 1.0 1.0;
	Printf.printf "draw begin.%d\n" (Texture.Resource_Map.cardinal (!texture_list));
try
	let (base, params) = Texture.Resource_Map.find "floor" (!texture_list) in
glBindTexture BindTex.GL_TEXTURE_2D base.tex_id;
(*	 	
  (* draw a pyramid (in smooth coloring mode) *)
  glEnd();                                      (* Done Drawing The Pyramid *)
*)



  glBegin GL_QUADS;                             (* start drawing the cube. *)

  (* top of cube *)
  glTexCoord2 0.0 1.0;                          (* Set The Color To Green *)
  glVertex3 ( 1.0) (1.0) (-1.0);                (* Top Right Of The Quad (Top) *)
  glTexCoord2 1.0 1.0;                          (* Set The Color To Green *)
  glVertex3 (-1.0) (1.0) (-1.0);                (* Top Left Of The Quad (Top) *)
  glTexCoord2 1.0 0.0;                          (* Set The Color To Green *)
  glVertex3 (-1.0) (1.0) ( 1.0);                (* Bottom Left Of The Quad (Top) *)
  glTexCoord2 0.0 0.0;                          (* Set The Color To Green *)
  glVertex3 ( 1.0) (1.0) ( 1.0);                (* Bottom Right Of The Quad (Top) *)

  (* bottom of cube *)
  glTexCoord2 0.0 1.0;                          (* Set The Color To Green *)
  glVertex3 ( 1.0) (-1.0) ( 1.0);               (* Top Right Of The Quad (Bottom) *)
  glTexCoord2 1.0 1.0;                          (* Set The Color To Green *)
  glVertex3 (-1.0) (-1.0) ( 1.0);               (* Top Left Of The Quad (Bottom) *)
  glTexCoord2 1.0 0.0;                          (* Set The Color To Green *)
  glVertex3 (-1.0) (-1.0) (-1.0);               (* Bottom Left Of The Quad (Bottom) *)
  glTexCoord2 0.0 0.0;                          (* Set The Color To Green *)
  glVertex3 ( 1.0) (-1.0) (-1.0);               (* Bottom Right Of The Quad (Bottom) *)

  (* front of cube *)
  glTexCoord2 0.0 1.0;                          (* Set The Color To Green *)
  glVertex3 ( 1.0) ( 1.0) (1.0);                (* Top Right Of The Quad (Front) *)
  glTexCoord2 1.0 1.0;                          (* Set The Color To Green *)
  glVertex3 (-1.0) ( 1.0) (1.0);                (* Top Left Of The Quad (Front) *)
  glTexCoord2 1.0 0.0;                          (* Set The Color To Green *)
  glVertex3 (-1.0) (-1.0) (1.0);                (* Bottom Left Of The Quad (Front) *)
  glTexCoord2 0.0 0.0;                          (* Set The Color To Green *)
  glVertex3 ( 1.0) (-1.0) (1.0);                (* Bottom Right Of The Quad (Front) *)

  (* back of cube *)
  glTexCoord2 0.0 1.0;                          (* Set The Color To Green *)
  glVertex3 ( 1.0) (-1.0) (-1.0);               (* Top Right Of The Quad (Back) *)
  glTexCoord2 1.0 1.0;                          (* Set The Color To Green *)
  glVertex3 (-1.0) (-1.0) (-1.0);               (* Top Left Of The Quad (Back) *)
  glTexCoord2 1.0 0.0;                          (* Set The Color To Green *)
  glVertex3 (-1.0) ( 1.0) (-1.0);               (* Bottom Left Of The Quad (Back) *)
  glTexCoord2 0.0 0.0;                          (* Set The Color To Green *)
  glVertex3 ( 1.0) ( 1.0) (-1.0);               (* Bottom Right Of The Quad (Back) *)

  (* left of cube *)
  glTexCoord2 0.0 1.0;                          (* Set The Color To Green *)
  glVertex3 (-1.0) ( 1.0) ( 1.0);               (* Top Right Of The Quad (Left) *)
  glTexCoord2 1.0 1.0;                          (* Set The Color To Green *)
  glVertex3 (-1.0) ( 1.0) (-1.0);               (* Top Left Of The Quad (Left) *)
  glTexCoord2 1.0 0.0;                          (* Set The Color To Green *)
  glVertex3 (-1.0) (-1.0) (-1.0);               (* Bottom Left Of The Quad (Left) *)
  glTexCoord2 0.0 0.0;                          (* Set The Color To Green *)
  glVertex3 (-1.0) (-1.0) ( 1.0);               (* Bottom Right Of The Quad (Left) *)

  (* Right of cube *)
  glTexCoord2 0.0 1.0;                          (* Set The Color To Green *)
  glVertex3 ( 1.0) ( 1.0) (-1.0);               (* Top Right Of The Quad (Right) *)
  glTexCoord2 1.0 1.0;                          (* Set The Color To Green *)
  glVertex3 ( 1.0) ( 1.0) ( 1.0);               (* Top Left Of The Quad (Right) *)
  glTexCoord2 1.0 0.0;                          (* Set The Color To Green *)
  glVertex3 ( 1.0) (-1.0) ( 1.0);               (* Bottom Left Of The Quad (Right) *)
  glTexCoord2 0.0 0.0;                          (* Set The Color To Green *)
  glVertex3 ( 1.0) (-1.0) (-1.0);               (* Bottom Right Of The Quad (Right) *)
  glEnd();                                      (* Done Drawing The Cube *) (* draw a cube (6 quadrilaterals) *)
(*
 glBegin GL_QUADS;                             (* start drawing the cube. *)

	glColor4 0.0 1.0 0.0 0.5;
  (* top of cube *)
  glVertex3 ( 1.0) (1.0) (-1.0);                (* Top Right Of The Quad (Top) *)
  glVertex3 (-1.0) (1.0) (-1.0);                (* Top Left Of The Quad (Top) *)
  glVertex3 (-1.0) (1.0) ( 1.0);                (* Bottom Left Of The Quad (Top) *)
  glVertex3 ( 1.0) (1.0) ( 1.0);                (* Bottom Right Of The Quad (Top) *)

  (* bottom of cube *)
  glVertex3 ( 1.0) (-1.0) ( 1.0);               (* Top Right Of The Quad (Bottom) *)
  glVertex3 (-1.0) (-1.0) ( 1.0);               (* Top Left Of The Quad (Bottom) *)
  glVertex3 (-1.0) (-1.0) (-1.0);               (* Bottom Left Of The Quad (Bottom) *)
  glVertex3 ( 1.0) (-1.0) (-1.0);               (* Bottom Right Of The Quad (Bottom) *)

  (* front of cube *)
  glVertex3 ( 1.0) ( 1.0) (1.0);                (* Top Right Of The Quad (Front) *)
  glVertex3 (-1.0) ( 1.0) (1.0);                (* Top Left Of The Quad (Front) *)
  glVertex3 (-1.0) (-1.0) (1.0);                (* Bottom Left Of The Quad (Front) *)
  glVertex3 ( 1.0) (-1.0) (1.0);                (* Bottom Right Of The Quad (Front) *)

  (* back of cube *)
  glVertex3 ( 1.0) (-1.0) (-1.0);               (* Top Right Of The Quad (Back) *)
  glVertex3 (-1.0) (-1.0) (-1.0);               (* Top Left Of The Quad (Back) *)
  glVertex3 (-1.0) ( 1.0) (-1.0);               (* Bottom Left Of The Quad (Back) *)
  glVertex3 ( 1.0) ( 1.0) (-1.0);               (* Bottom Right Of The Quad (Back) *)

  (* left of cube *)
  glVertex3 (-1.0) ( 1.0) ( 1.0);               (* Top Right Of The Quad (Left) *)
  glVertex3 (-1.0) ( 1.0) (-1.0);               (* Top Left Of The Quad (Left) *)
  glVertex3 (-1.0) (-1.0) (-1.0);               (* Bottom Left Of The Quad (Left) *)
  glVertex3 (-1.0) (-1.0) ( 1.0);               (* Bottom Right Of The Quad (Left) *)

  (* Right of cube *)
  glVertex3 ( 1.0) ( 1.0) (-1.0);               (* Top Right Of The Quad (Right) *)
  glVertex3 ( 1.0) ( 1.0) ( 1.0);               (* Top Left Of The Quad (Right) *)
  glVertex3 ( 1.0) (-1.0) ( 1.0);               (* Bottom Left Of The Quad (Right) *)
  glVertex3 ( 1.0) (-1.0) (-1.0);               (* Bottom Right Of The Quad (Right) *)
  glEnd();                                      (* Done Drawing The Cube *)
*)
  rtri := !rtri +. 15.0;                        (* Increase The Rotation Variable For The Pyramid *)
  rquad := !rquad +. 1.0;                      (* Decrease The Rotation Variable For The Cube *)

  (* swap the buffers to display, since double buffering is used. *)
  glutSwapBuffers();
with
	Not_found -> raise Not_found
;;


(* The function called whenever a key is pressed. *)
let keyPressed ~window ~key ~x ~y =

  (* If escape is pressed, kill everything. *)
  if key = '\027' then
  begin
    (* shut down our window *)
    glutDestroyWindow window;

    (* exit the program...normal termination. *)
    exit 0;
  end;
;;


let () =
  	ignore(glutInit Sys.argv);

  	glutInitDisplayMode [GLUT_RGBA; GLUT_DOUBLE; GLUT_ALPHA; GLUT_DEPTH];

  	glutInitWindowSize 640 480;

  	glutInitWindowPosition 0 0;

  	let window = glutCreateWindow "Light Propagation Volume" in

  	glutDisplayFunc drawGLScene;

 	(*glutFullScreen();*)

  	glutIdleFunc drawGLScene;

  	glutReshapeFunc reSizeGLScene;

  	glutKeyboardFunc (keyPressed ~window);

  	initGL 640 480;

  	glutMainLoop();
;;

