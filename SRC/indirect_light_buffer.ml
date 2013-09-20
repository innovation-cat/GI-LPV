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
		mutable indirect_light_rt     : Render_texture.t;
		mutable light_buffer 	      : GL.texture_id;
		mutable depth_buffer 	      : FBO.rbo_id;
		mutable blur_rt   	      : Render_texture.t;
		mutable blur_buffer           : GL.texture_id;
		mutable dampen                : bool;
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
	let blur_buffer_base = {Texture.tex_id = GL.glGenTexture (); Texture.target = GL.BindTex.GL_TEXTURE_2D; Texture.name = "blur_buffer"} in
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
		dampen=true;
	}
;;

let set_begin i_l_b view_mat proj_mat sun_light light_texture_dim = 
	let shader = if i_l_b.dampen = true then 
			Glsl_shader.Resource_Map.find "indirect_light1" (!Glsl_shader.shader_list)
		     else 
			Glsl_shader.Resource_Map.find "indirect_light0" (!Glsl_shader.shader_list)
	in
	
	FBO.glBindFrameBuffer FBO.GL_FRAMEBUFFER i_l_b.indirect_light_rt.Render_texture.id;
	
	GL.glViewport 0 0 i_l_b.width i_l_b.height;
	GL.glClearColor 0.0 0.0 0.0 0.0;
	glClear [GL.GL_COLOR_BUFFER_BIT; GL.GL_DEPTH_BUFFER_BIT];
	
	let program = shader.Glsl_shader.program in
	GL.glUseProgram program;
	
	let loc = GL.glGetUniformLocation program "projection_matrix" in
	if loc = -1 then raise (Failure "load projection_matrix failure.");
	GL.glUniformMatrix4f loc false proj_mat;

	let loc = GL.glGetUniformLocation program "view_matrix" in
	if loc = -1 then raise (Failure "load view_matrix failure.");
	GL.glUniformMatrix4f loc false view_mat;

	let grid_space_rotation = sun_light.Directional_light.grid_space.Directional_light.rotation in
	let loc = GL.glGetUniformLocation program "grid_space_matrix" in
	if loc = -1 then raise (Failure "load grid_space_matrix failure.");
	GL.glUniformMatrix4f loc false grid_space_rotation;

	let Vector.Vec3 (grid_size_x, grid_size_y, grid_size_z) = Bounding_box.calc_dim sun_light.Directional_light.grid_bbox in
	let Vector.Vec3 (inv_grid_size_x, inv_grid_size_y, inv_grid_size_z) = Vector.Vec3 (1.0/.grid_size_x, 1.0/.grid_size_y, 1.0/.grid_size_z) in
	let loc = GL.glGetUniformLocation program "inv_grid_size" in
	if loc = -1 then raise (Failure "load inv_grid_size failure.");
	GL.glUniform3f loc inv_grid_size_x inv_grid_size_y inv_grid_size_z;

	if i_l_b.dampen = true then begin
		let Vector.Vec3 (x, y, z) = light_texture_dim in
		let offset_along_normal = Vector.Vec3 (1.0/.x, 1.0/.y, 1.0/.z) in
		let Vector.Vec3 (x, y, z) = Vector.scale offset_along_normal 0.866 in
		
		let loc = GL.glGetUniformLocation program "offset_along_normal" in
		if loc = -1 then raise (Failure "load offset_along_normal failure.");
		GL.glUniform3f loc x y z;
	end;

	let Vector.Vec3 (x, y, z) = sun_light.Directional_light.grid_bbox.Bounding_box.min in
	let loc = GL.glGetUniformLocation program "grid_origin" in
	if loc = -1 then raise (Failure "load grid_origin failure.");
	GL.glUniform3f loc x y z;
	
	let loc = GL.glGetUniformLocation program "incoming_red" in
	if loc = -1 then raise (Failure "load incoming_red failure.");
	GL.glUniform1i loc 0;

	let loc = GL.glGetUniformLocation program "incoming_green" in
	if loc = -1 then raise (Failure "load incoming_green failure.");
	GL.glUniform1i loc 1;

	let loc = GL.glGetUniformLocation program "incoming_blue" in
	if loc = -1 then raise (Failure "load incoming_blue failure.");
	GL.glUniform1i loc 2;
;;


let draw i_l_b test_model =
	let shader = if i_l_b.dampen = true then 
			Glsl_shader.Resource_Map.find "indirect_light1" (!Glsl_shader.shader_list)
		     else 
			Glsl_shader.Resource_Map.find "indirect_light0" (!Glsl_shader.shader_list)
	in

	GL_buffer.bind_vertex_buffer_with_shader test_model.Model.vb_info shader;
	VertArray.glDrawArrays GL.GL_TRIANGLES 0 test_model.Model.vb_info.GL_buffer.length;
	GL_buffer.unbind_vertex_buffer_with_shader test_model.Model.vb_info;
;;

let set_end i_l_b = FBO.glUnBindFrameBuffer FBO.GL_FRAMEBUFFER;;


let blur_aux i_l_b light_texture_dim sun_light proj vertical shader program =
	let bbox = sun_light.Directional_light.grid_bbox in
	let Vector.Vec3 (bbox_size_x, bbox_size_y, bbox_size_z) = Bounding_box.calc_dim bbox in	
	let Vector.Vec3 (light_texture_dim_x, light_texture_dim_y, light_texture_dim_z) = light_texture_dim in
	let cell_size = bbox_size_x /. light_texture_dim_x in
	let cell_size_scale = 0.5 in
	let dist = cell_size *. cell_size_scale *. 0.5 in
	let dist_threashold = cell_size *. cell_size *. 3.0 in

	let loc = GL.glGetUniformLocation program "dist_threashold" in
	if loc = -1 then raise (Failure "load dist_threashold failure.");
	GL.glUniform4f loc dist_threashold dist_threashold dist_threashold dist_threashold;
	
	let Vector.Vec2 (x, y) = proj in
	let Vector.Vec2 (grid_to_texture_scale_x, grid_to_texture_scale_y) = if vertical = true then Vector.Vec2 (0.0, dist *. y /. 2.0)
									     else Vector.Vec2 (dist *. x /. 2.0, 0.0)
	in
	let loc = GL.glGetUniformLocation program "grid_to_texture_scale" in
	if loc = -1 then raise (Failure "load grid_to_texture_scale failure.");
	GL.glUniform2f loc grid_to_texture_scale_x grid_to_texture_scale_y;

	let vb_info = GL_buffer.Resource_Map.find "fullscreenquad"  (!GL_buffer.buffer_list) in
	(* bind *)
	GL_buffer.bind_vertex_buffer_with_shader vb_info shader;
	
	(* draw *)
	VertArray.glDrawArrays GL.GL_TRIANGLES 0 vb_info.GL_buffer.length;

	(* unbind *)
	GL_buffer.unbind_vertex_buffer_with_shader vb_info;
;;
	

let blur i_l_b d_n_b light_texture_dim sun_light = 
	(* blur pass begin *)
	GL.glDepthMask false;
	GL.glDisable GL.GL_DEPTH_TEST;
	
	GL.glViewport 0 0 i_l_b.width i_l_b.height;
	let shader = Glsl_shader.Resource_Map.find "blur" (!Glsl_shader.shader_list) in
	let program = shader.Glsl_shader.program in
	GL.glUseProgram program;

	let loc = GL.glGetUniformLocation program "indirect_light_tex" in
	if loc = -1 then raise (Failure "load indirect_light_tex failure.");
	GL.glUniform1i loc 0;
	
	let loc = GL.glGetUniformLocation program "depth_normal_tex" in
	if loc = -1 then raise (Failure "load depth_normal_tex failure.");
	GL.glUniform1i loc 1;

	let Vector.Vec2 (x, y) = d_n_b.Depth_normal_buffer.near_far_plane in
	let loc = GL.glGetUniformLocation program "near_far_plane" in
	if loc = -1 then raise (Failure "load near_far_plane failure.");
	GL.glUniform2f loc x y;

	let Vector.Vec2 (x, y) = d_n_b.Depth_normal_buffer.inv_proj in
	let loc = GL.glGetUniformLocation program "inv_proj" in
	if loc = -1 then raise (Failure "load inv_proj failure.");
	GL.glUniform2f loc x y;

	
	let (depth_normal_base, depth_normal_params) = match Texture.Resource_Map.find "depth_normal" (!Texture.texture_list) with 
							   Texture.Texture_2D (base, params) -> (base, params) 
							|  _ -> raise (Failure "rsm_color_tex should be 2-dimension texture.")
	in
	GL.glActiveTexture GL.GL_TEXTURE1;
	GL.glBindTexture depth_normal_base.Texture.target depth_normal_base.Texture.tex_id;

	(*************************************************************************************************************************)
	(* vertical blur pass                                                                                                    *)
	let (indirect_light_buffer_base, indirect_light_buffer_params) = match Texture.Resource_Map.find "indirect_light_buffer" (!Texture.texture_list) with 
							   Texture.Texture_2D (base, params) -> (base, params) 
							|  _ -> raise (Failure "indirect_light_buffer should be 2-dimension texture.")
	in
	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glBindTexture indirect_light_buffer_base.Texture.target indirect_light_buffer_base.Texture.tex_id;

	FBO.glBindFrameBuffer FBO.GL_FRAMEBUFFER i_l_b.blur_rt.Render_texture.id;
	
	let Vector.Vec2 (x, y) = d_n_b.Depth_normal_buffer.inv_proj in
	let proj = Vector.Vec2 (1.0/.x, 1.0/.y) in
	
	blur_aux i_l_b light_texture_dim sun_light proj true shader program;

	FBO.glUnBindFrameBuffer FBO.GL_FRAMEBUFFER;

	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glUnbindTexture indirect_light_buffer_base.Texture.target;

	(************************************************************************************************************************)
	(* horizontal blur pass                                                                                                 *)
	let (blur_buffer_base, blur_buffer_params) = match Texture.Resource_Map.find "blur_buffer" (!Texture.texture_list) with 
							   Texture.Texture_2D (base, params) -> (base, params) 
							|  _ -> raise (Failure "indirect_light_buffer should be 2-dimension texture.")
	in
	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glBindTexture blur_buffer_base.Texture.target blur_buffer_base.Texture.tex_id;

	FBO.glBindFrameBuffer FBO.GL_FRAMEBUFFER i_l_b.indirect_light_rt.Render_texture.id;
	
	let Vector.Vec2 (x, y) = d_n_b.Depth_normal_buffer.inv_proj in
	let proj = Vector.Vec2 (1.0/.x, 1.0/.y) in
	
	blur_aux i_l_b light_texture_dim sun_light proj false shader program;

	FBO.glUnBindFrameBuffer FBO.GL_FRAMEBUFFER;

	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glUnbindTexture blur_buffer_base.Texture.target;

	(* blur end *)
	GL.glDepthMask true;
	GL.glEnable GL.GL_DEPTH_TEST;
;;
