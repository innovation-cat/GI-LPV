(* depth normal buffer required to inject blocking potentials into geometry volume *)
open GL
open FBO
open Glex
open Depth_buffer
open Render_texture
open Vector
open Glsl_shader

type t = {
		mutable width                 : int;
		mutable height                : int;
	(*	mutable rt           	      : FBO.fbo_id;*)
		mutable light_buffer 	      : GL.texture_id;
		mutable depth_buffer 	      : FBO.rbo_id;
	(*	mutable blocker_rt   	      : FBO.fbo_id;*)
		mutable blur_buffer           : GL.texture_id;
	 }

let create width height = 
	(* create indirect light buffer texture *)
	let indirect_light_buffer_base = {Texture.tex_id = GL.glGenTexture (); Texture.target = GL.BindTex.GL_TEXTURE_2D; Texture.name = "indirect_light_buffer"} in
	let indirect_light_buffer_params = {Texture.init_param_2d with Texture.Texture_Params_2D.mag_filter = GL.Mag.GL_LINEAR; Texture.Texture_Params_2D.min_filter = GL.Min.GL_LINEAR; Texture.Texture_Params_2D.internal_format = Glex.GL_RGB8} in
	Texture.create_texture_2d indirect_light_buffer_base indirect_light_buffer_params width height None;

	(* blur buffer texture is the same as indirect light buffer *)
	let blur_buffer_base = {indirect_light_buffer_base with Texture.name = "blur_buffer"} in
	let blur_buffer_params = indirect_light_buffer_params in 
	Texture.create_texture_2d blur_buffer_base blur_buffer_params width height None;

	let zbuffer_params = Depth_buffer.init_depth_buffer_param in
	let depthbuffer = Depth_buffer.create zbuffer_params width height in
	let textures =  {
				tex_list = [|indirect_light_buffer_base.Texture.tex_id|];
				depth_tex_list = [||];
			}
	in
	Render_texture.create_render_texture textures (Some depthbuffer);

	let textures = {
				tex_list = [| blur_buffer_base.Texture.tex_id |];
				depth_tex_list = [||];
		       }
	in
	Render_texture.create_render_texture textures None;
	{
		width; height; 
		light_buffer = indirect_light_buffer_base.Texture.tex_id;
		depth_buffer = depthbuffer.Depth_buffer.id;
		blur_buffer = blur_buffer_base.Texture.tex_id;
	}
;;

let create_indirect_light_shader0 () = 
	(* indirect light shader *)
	let vertex_shader = glCreateShader GL_VERTEX_SHADER in
	let sc = open_in "../shader/indirect_light.vp" in
	let size = in_channel_length sc in
	let vertex_shader_src = String.create size in
	ignore (input sc vertex_shader_src 0 size);
	Printf.printf "\n\n%s\n\n" vertex_shader_src;
	glShaderSource vertex_shader vertex_shader_src;
	close_in sc;

	let fragment_shader = glCreateShader GL_FRAGMENT_SHADER in
	let sc = open_in "../shader/indirect_light.fp" in
	let size = in_channel_length sc in
	let fragment_shader_src = String.create size in
	ignore (input sc fragment_shader_src 0 size);
	Printf.printf "\n\n%s\n\n" fragment_shader_src;
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
			   {Glsl_shader.name = "position"; Glsl_shader.value = glGetAttribLocation program "position"} |]
	in
	let geometry_shader = glCreateShader GL_GEOMETRY_SHADER in
	let shader_info = {vertex_shader; fragment_shader; geometry_shader; program;attributes} in
	Glsl_shader.insert_shader_list "indirect_light0" shader_info
;;

let create_indirect_light_shader1 () = 
	let vertex_shader = glCreateShader GL_VERTEX_SHADER in
	let sc = open_in "../shader/indirect_light.vp" in
	let size = in_channel_length sc in
	let vertex_shader_src = String.create size in
	ignore (input sc vertex_shader_src 0 size);
	Printf.printf "\n\n%s\n\n" vertex_shader_src;
	let sources = [|"#define DAMPEN"; vertex_shader_src|] in
	Glex.glShaderSources vertex_shader 2 (Some sources) None;
	close_in sc;

	let fragment_shader = glCreateShader GL_FRAGMENT_SHADER in
	let sc = open_in "../shader/indirect_light.fp" in
	let size = in_channel_length sc in
	let fragment_shader_src = String.create size in
	ignore (input sc fragment_shader_src 0 size);
	Printf.printf "\n\n%s\n\n" fragment_shader_src;
	let sources = [|"#define DAMPEN"; fragment_shader_src|] in
	Glex.glShaderSources fragment_shader 2 (Some sources) None;
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
			   {Glsl_shader.name = "position"; Glsl_shader.value = glGetAttribLocation program "position"}|];
	in
	
	let geometry_shader = glCreateShader GL_GEOMETRY_SHADER in
	let shader_info = {vertex_shader; fragment_shader; geometry_shader; program;attributes} in
	Glsl_shader.insert_shader_list "indirect_light1" shader_info
;;

let create_blur_shader () =
	(* load blur shader source *)
	let vertex_shader = glCreateShader GL_VERTEX_SHADER in
	let sc = open_in "../shader/blur.vp" in
	let size = in_channel_length sc in
	let vertex_shader_src = String.create size in
	ignore (input sc vertex_shader_src 0 size);
	Printf.printf "\n\n%s\n\n" vertex_shader_src;
	glShaderSource vertex_shader vertex_shader_src;
	close_in sc;

	let fragment_shader = glCreateShader GL_FRAGMENT_SHADER in
	let sc = open_in "../shader/blur.fp" in
	let size = in_channel_length sc in
	let fragment_shader_src = String.create size in
	ignore (input sc fragment_shader_src 0 size);
	Printf.printf "\n\n%s\n\n" fragment_shader_src;
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
	let attributes = [| {Glsl_shader.name = "position"; Glsl_shader.value = glGetAttribLocation program "position"} |] in
	let geometry_shader = glCreateShader GL_GEOMETRY_SHADER in
	let shader_info = {vertex_shader; fragment_shader; geometry_shader; program;attributes} in
	Glsl_shader.insert_shader_list "blur" shader_info;
;;	

let create_shader () = 
	Printf.printf "begin.\n";
	flush stdout;
	create_indirect_light_shader0 ();
	create_indirect_light_shader1 ();
	create_blur_shader ()
;;
