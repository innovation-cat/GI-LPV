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
		mutable rt           	      : Render_texture.t;
		mutable depth_normal 	      : GL.texture_id option;
		mutable depth_buffer 	      : FBO.rbo_id option;
		mutable blocker_rt   	      : Render_texture.t;
		mutable blocker_depth_normal  : GL.texture_id option;
		mutable blocker_buffer_width  : int;
		mutable blocker_buffer_height : int;
		mutable near_far_plane        : Vector.vec;
		mutable from_view_to_grid    : float array;
		mutable inv_proj              : Vector.vec ;
	}

(*
let init_depth_normal_buffer = {width=0; height=0; blocker_buffer_width=0; blocker_buffer_height=0; depth_normal=None;
				blocker_depth_normal=None; depth_buffer=None; blocker_rt=None; rt=None; 
				near_far_plane=Vector.Vec3 (0.0, 0.0, 0.0)}
;;
*)

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
	let rt = Render_texture.create textures (Some depthbuffer) in

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
	let blocker_rt = Render_texture.create textures None in
	create_shader ();
	{
		width; height; blocker_buffer_width; blocker_buffer_height; near_far_plane;
		depth_normal = Some depth_normal_base.Texture.tex_id;
		depth_buffer = Some depthbuffer.Depth_buffer.id;
		blocker_depth_normal = Some blocker_depth_normal_base.Texture.tex_id;
		rt; blocker_rt;
		inv_proj = Vector.Vec2 (0.0, 0.0);
		from_view_to_grid = Vector.build_identity_matrix ();
	}
;;

let set_begin d_n_b view_matrix proj_matrix sun_light = 
	(* bind frame buffer object *)
	FBO.glBindFrameBuffer FBO.GL_FRAMEBUFFER d_n_b.rt.Render_texture.id;
	
	(* load shader program *)
	let shader = Glsl_shader.Resource_Map.find "depth_normal" (!Glsl_shader.shader_list) in
	let program = shader.Glsl_shader.program in
	GL.glUseProgram program;
	
	let loc = GL.glGetUniformLocation program "projection_matrix" in
	if loc = -1 then raise (Failure "load projection_matrix variable failure.");
	GL.glUniformMatrix4f loc false proj_matrix;
	
	let loc = GL.glGetUniformLocation program "view_matrix" in
	if loc = -1 then raise (Failure "load view_matrix variable failure.");
	GL.glUniformMatrix4f loc false view_matrix;

	let loc = GL.glGetUniformLocation program "near_far_plane" in
	if loc = -1 then raise (Failure "load projection_matrix variable failure.");
	let (x, y) = match d_n_b.near_far_plane with 
		   Vector.Vec2 (a, b) -> (a, b)
		|  _ -> raise (Failure "near_far_plane is a 2-dimension vector.")
	in
	GL.glUniform2f loc x y;
	
	let (x, _, _) = match Vector.get_x_axis proj_matrix with 
		   Vector.Vec3 (a, b, c) -> (a, b, c)
		|  _ -> raise (Failure "unexpected error.")
	in
	let (_, y, _) = match Vector.get_y_axis proj_matrix with 
		   Vector.Vec3 (a, b, c) -> (a, b, c)
		|  _ -> raise (Failure "unexpected error.")
	in
	d_n_b.inv_proj <- Vector.Vec2 (1.0/.x, 1.0/.y);
	
	let to_world = Vector.inverse view_matrix in
	let grid_space_rotation = sun_light.Directional_light.grid_space.Directional_light.rotation in
	d_n_b.from_view_to_grid <- Vector.mult grid_space_rotation to_world;
	
	GL.glViewport 0 0 d_n_b.width d_n_b.height; 
	GL.glClearColor 1.0 1.0 1.0 1.0;
	glClear [GL.GL_COLOR_BUFFER_BIT; GL.GL_DEPTH_BUFFER_BIT];
;;

let draw d_n_b test_model = 
	let vertex_buffer = test_model.Model.vb_info in
	let shader = Glsl_shader.Resource_Map.find "depth_normal" (!Glsl_shader.shader_list) in
	GL_buffer.bind_vertex_buffer_with_shader vertex_buffer shader;
	
	VertArray.glDrawArrays GL.GL_TRIANGLES 0 150;
	GL_buffer.unbind_vertex_buffer_with_shader vertex_buffer;
;;

let set_end d_n_b = 
	FBO.glUnBindFrameBuffer FBO.GL_FRAMEBUFFER;
	
	(* create blocker buffer *)
	GL.glDepthMask false;
	GL.glDisable GL.GL_DEPTH_TEST;
	
	FBO.glBindFrameBuffer FBO.GL_FRAMEBUFFER d_n_b.blocker_rt.Render_texture.id;
	let shader = Glsl_shader.Resource_Map.find "resample" (!Glsl_shader.shader_list) in
	let program = shader.Glsl_shader.program in
	GL.glUseProgram program;
	
	let (depth_normal_base, depth_normal_params) = match Texture.Resource_Map.find "depth_normal" (!Texture.texture_list) with 
		   Texture.Texture_2D (base, params) -> (base, params)
		|  _ -> raise (Failure "depth_normal is a 2-dimension texture.")
	in
	let loc = GL.glGetUniformLocation program "texture" in
	if loc = -1 then raise (Failure "load projection_matrix variable failure.");
	GL.glUniform1i loc 0;
	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glBindTexture depth_normal_base.Texture.target depth_normal_base.Texture.tex_id;
	glViewport 0 0 d_n_b.blocker_buffer_width d_n_b.blocker_buffer_height;

	let vertices = [|1.0; -1.0; 0.0; 1.0; 1.0; 0.0; -1.0; 1.0; 0.0; 1.0; -1.0; 0.0; -1.0; 1.0; 0.0; -1.0; -1.0; -1.0|] in
	let decl_array = [|{GL_buffer.field = GL_buffer.VEC3F; GL_buffer.binding_name = "position"}|] in
	let vb_info = GL_buffer.create_vertex_buffer "fullscreenquad" decl_array 6 VBO.GL_STATIC_DRAW in
	Printf.printf "%d\n" vb_info.GL_buffer.buffer_size;
	GL_buffer.insert_buffer_list "fullscreenquad" vb_info;
	GL_buffer.bind_vertex_buffer vb_info;
	GL_buffer.set_vertex_buffer_data 0 vb_info.GL_buffer.buffer_size vertices;
	GL_buffer.unbind_vertex_buffer_with_shader vb_info;

	(* bind *)
	GL_buffer.bind_vertex_buffer_with_shader vb_info shader;
	
	(* draw *)
	VertArray.glDrawArrays GL.GL_TRIANGLES 0 6;

	(* unbind *)
	GL_buffer.unbind_vertex_buffer_with_shader vb_info;
	
	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glUnbindTexture depth_normal_base.Texture.target;
	
	FBO.glUnBindFrameBuffer FBO.GL_FRAMEBUFFER;

	GL.glDepthMask true;
	GL.glEnable GL.GL_DEPTH_TEST;
;;
