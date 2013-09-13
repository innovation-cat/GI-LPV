(* depth normal buffer required to inject blocking potentials into geometry volume *)
open GL
open FBO
open Depth_buffer
open Render_texture
open Vector
open Glsl_shader

type t = {
		mutable width                 : int;
		mutable height                : int;
	(*	mutable rt           	      : FBO.fbo_id;*)
		mutable depth_normal 	      : GL.texture_id;
		mutable depth_buffer 	      : FBO.rbo_id;
	(*	mutable blocker_rt   	      : FBO.fbo_id;*)
		mutable blocker_depth_normal  : GL.texture_id;
		mutable blocker_buffer_width  : int;
		mutable blocker_buffer_height : int;
		mutable near_far_plane        : Vector.vec
	 }

let create width height near_plane far_plane blocker_buffer_width = 
	let near_far_plane = Vec2 (near_plane, far_plane -. near_plane) in
	let depth_normal_base = {Texture.tex_id = GL.glGenTexture (); Texture.target = GL.BindTex.GL_TEXTURE_2D; Texture.name = "depth_normal"} in
	let depth_normal_params = {Texture.init_param_2d with Texture.Texture_Params_2D.mag_filter = GL.Mag.GL_NEAREST; Texture.Texture_Params_2D.min_filter = GL.Min.GL_NEAREST; Texture.Texture_Params_2D.internal_format = Glex.GL_RGBA8} in
	Texture.create_texture_2d depth_normal_base depth_normal_params width height None;

	let zbuffer_params = Depth_buffer.init_depth_buffer_param in
	let depthbuffer = Depth_buffer.create zbuffer_params width height in
	let textures =  {
				tex_list = [|depth_normal_base.Texture.tex_id|];
				depth_tex_list = [||];
			}
	in
	Render_texture.create textures (Some depthbuffer);

	let aspect = (float width) /. (float height) in
	let blocker_buffer_height = Int32.to_int (Int32.of_float ((float blocker_buffer_width) /. aspect +. 0.5)) in
	
	let blocker_depth_normal_base = {Texture.tex_id = GL.glGenTexture (); Texture.target = GL.BindTex.GL_TEXTURE_2D; Texture.name = "blocker_depth_normal"} in
	let blocker_depth_normal_params = depth_normal_params in
	Texture.create_texture_2d blocker_depth_normal_base blocker_depth_normal_params blocker_buffer_width blocker_buffer_height None;

	let textures = {
				tex_list = [|blocker_depth_normal_base.Texture.tex_id|];
				depth_tex_list = [||];
		       }
	in
	Render_texture.create textures None;
	{
		width; height; blocker_buffer_width; blocker_buffer_height; near_far_plane;
		depth_normal = depth_normal_base.Texture.tex_id;
		depth_buffer = depthbuffer.Depth_buffer.id;
		blocker_depth_normal = blocker_depth_normal_base.Texture.tex_id;
	}

let create_shader () = 
	let depth_normal_vertex_shader_file = "../shader/depth_normal.vp" in
	let depth_normal_fragment_shader_file = "../shader/depth_normal.fp" in
	let shader_info = Glsl_shader.create depth_normal_vertex_shader_file depth_normal_fragment_shader_file None None None in
	Glsl_shader.insert_shader_list "depth_normal" shader_info;

	let resample_vertex_shader_file = "../shader/resample.vp" in
	let resample_fragment_shader_file = "../shader/resample.fp" in
	let shader_info = Glsl_shader.create resample_vertex_shader_file resample_fragment_shader_file None None None in
	Glsl_shader.insert_shader_list "resample" shader_info
;;
	(* 
	let vertex_shader = glCreateShader GL_VERTEX_SHADER in
	let sc = open_in "../shader/depth_normal.vp" in
	let size = in_channel_length sc in
	let vertex_shader_src = String.create size in
	ignore (input sc vertex_shader_src 0 size);
	glShaderSource vertex_shader vertex_shader_src;
	close_in sc;

	let fragment_shader = glCreateShader GL_FRAGMENT_SHADER in
	let sc = open_in "../shader/depth_normal.fp" in
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
			   {Glsl_shader.name = "position"; Glsl_shader.value = glGetAttribLocation program "position"} |]
	in
	let geometry_shader = glCreateShader GL_GEOMETRY_SHADER in
	let shader_info = {vertex_shader; fragment_shader; geometry_shader; program;attributes} in
	Glsl_shader.insert_shader_list "depth_normal" shader_info;

	(* load resample shader source *)
	let vertex_shader = glCreateShader GL_VERTEX_SHADER in
	let sc = open_in "../shader/resample.vp" in
	let size = in_channel_length sc in
	let vertex_shader_src = String.create size in
	ignore (input sc vertex_shader_src 0 size);
	glShaderSource vertex_shader vertex_shader_src;
	close_in sc;

	let fragment_shader = glCreateShader GL_FRAGMENT_SHADER in
	let sc = open_in "../shader/resample.fp" in
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
	let attributes = [| {Glsl_shader.name = "position"; Glsl_shader.value = glGetAttribLocation program "position"} |] in
	let geometry_shader = glCreateShader GL_GEOMETRY_SHADER in
	let shader_info = {vertex_shader; fragment_shader; geometry_shader; program;attributes} in
	Glsl_shader.insert_shader_list "resample" shader_info;
*)	
