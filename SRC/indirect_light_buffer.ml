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
		mutable indirect_light_rt     : FBO.fbo_id;
		mutable light_buffer 	      : GL.texture_id;
		mutable depth_buffer 	      : FBO.rbo_id;
		mutable blur_rt   	      : FBO.fbo_id;
		mutable blur_buffer           : GL.texture_id;
	 }


let create_shader () =
	let indirect_light_vertex_shader0 = "../shader/indirect_light.vp" in
	let indirect_light_fragment_shader0 = "../shader/indirect_light.fp" in
	let shader_info = Glsl_shader.create indirect_light_vertex_shader0 indirect_light_fragment_shader0 None None None in
	Glsl_shader.insert_shader_list "indirect_light0" shader_info;

	let indirect_light_vertex_shader1 = "../shader/indirect_light.vp" in
	let indirect_light_fragment_shader1 = "../shader/indirect_light.fp" in
	let shader_info = Glsl_shader.create indirect_light_vertex_shader1 indirect_light_fragment_shader1 None None (Some "#define DAMPEN") in
	Glsl_shader.insert_shader_list "indirect_light1" shader_info;
	
	let blur_vertex_shader = "../shader/blur.vp" in
	let blur_fragment_shader = "../shader/blur.fp" in
	let shader_info = Glsl_shader.create blur_vertex_shader blur_fragment_shader None None None in
	Glsl_shader.insert_shader_list "blur" shader_info
;;


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
	let indirect_light_rt = Render_texture.create textures (Some depthbuffer) in

	let textures = {
				tex_list = [| blur_buffer_base.Texture.tex_id |];
				depth_tex_list = [||];
		       }
	in
	let blur_rt = Render_texture.create textures None in
	create_shader ();
	{
		width; height; 
		light_buffer = indirect_light_buffer_base.Texture.tex_id;
		depth_buffer = depthbuffer.Depth_buffer.id;
		blur_buffer = blur_buffer_base.Texture.tex_id;
		indirect_light_rt; blur_rt;
	}
;;
