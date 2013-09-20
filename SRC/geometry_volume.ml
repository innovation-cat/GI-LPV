type t = {
		mutable dim_x 			: int;
		mutable dim_y 			: int;
		mutable dim_z 			: int;
		mutable cell_size               : Vector.vec;
		mutable bbox 			: Bounding_box.bounding_box;
		mutable tex   			: GL.texture_id;
		mutable fbo   			: Render_texture.t;
	}


let injected = ref false

let create_shader () = 
	let inject_blocker_vertex_shader_file = "../shader/inject_blocker.vp" in
	let inject_blocker_fragment_shader_file = "../shader/inject_blocker.fp" in
	let inject_blocker_geometry_shader_file = "../shader/inject_blocker.gp" in
	let geometry_params = {Glsl_shader.input_type = 0; Glsl_shader.output_type = 0; Glsl_shader.vertices_out = 1} in
	let shader = Glsl_shader.create inject_blocker_vertex_shader_file inject_blocker_fragment_shader_file (Some inject_blocker_geometry_shader_file) (Some geometry_params) None in
	Glsl_shader.insert_shader_list "inject_blocker" shader;

	let inject_blocker2_vertex_shader_file = "../shader/inject_blocker2.vp" in
	let inject_blocker2_fragment_shader_file = "../shader/inject_blocker2.fp" in
	let inject_blocker2_geometry_shader_file = "../shader/inject_blocker2.gp" in
	let geometry_params = {Glsl_shader.input_type = 0; Glsl_shader.output_type = 0; Glsl_shader.vertices_out = 1} in
	let inject_blockers_shader = Glsl_shader.create inject_blocker2_vertex_shader_file inject_blocker2_fragment_shader_file (Some inject_blocker2_geometry_shader_file) (Some geometry_params) None in
	Glsl_shader.insert_shader_list "inject_blocker2" inject_blockers_shader;
	
	let select_gv_vertex_shader_file = "../shader/propagate.vp" in
	let select_gv_fragment_shader_file = "../shader/select_gv.fp" in
	let select_gv_geometry_shader_file = "../shader/propagate.gp" in
	let geometry_params = {Glsl_shader.input_type = 4; Glsl_shader.output_type = 5; Glsl_shader.vertices_out = 3} in
	let gv_select_shader = Glsl_shader.create select_gv_vertex_shader_file select_gv_fragment_shader_file (Some select_gv_geometry_shader_file) (Some geometry_params) None in
	Glsl_shader.insert_shader_list "select_gv" gv_select_shader;
;;	

let create_gv_texture tex_name dim_x dim_y dim_z pixels = 
	let tex_params = {Texture.init_param_3d with Texture.Texture_Params_3D.mag_filter = GL.Mag.GL_NEAREST;
						     Texture.Texture_Params_3D.min_filter = GL.Min.GL_NEAREST;
						     Texture.Texture_Params_3D.internal_format = Glex.GL_RGBA16F;
						     Texture.Texture_Params_3D.source_format = GL.GL_RGBA;
						     Texture.Texture_Params_3D.n_type = GL.GL_FLOAT
			 }
	in
	let tex_base = {Texture.tex_id = GL.glGenTexture (); Texture.target = GL.BindTex.GL_TEXTURE_3D; Texture.name = tex_name} in
	Texture.create_texture_3d tex_base tex_params dim_x dim_y dim_z pixels;
	tex_base.Texture.tex_id 
;;
	
let create cell_size dim_x dim_y dim_z =
	let bbox = Bounding_box.init in
	let Vector.Vec3 (x, y, z) = cell_size in
	let x = x *. (float dim_x) *. 0.5 and y = y *. (float dim_y) *. 0.5 and z = z *. (float dim_z) *. 0.5 in
	ignore (Bounding_box.add_vertex (x, y, z) bbox);
	ignore (Bounding_box.add_vertex (-1. *. x, -1. *. y, -1. *. z) bbox);
	create_shader ();
	let tex = create_gv_texture "gv_texture0" dim_x dim_y dim_z None in
	let textures = {Render_texture.tex_list = [|tex|]; Render_texture.depth_tex_list = [||]} in
	let fbo = Render_texture.create textures None in 
	{dim_x; dim_y; dim_z; cell_size; bbox; tex; fbo;}
;;

let copy_gv0 gv = 
	let tex = create_gv_texture "gv_texture1" gv.dim_x gv.dim_y gv.dim_z None in
	let textures = {Render_texture.tex_list = [|tex|]; Render_texture.depth_tex_list = [||]} in
	let fbo = Render_texture.create textures None in 
	{ dim_x = gv.dim_x; 
	  dim_y = gv.dim_y; 
	  dim_z = gv.dim_z;
	  cell_size = gv.cell_size;
	  bbox = gv.bbox; 
	  tex; fbo;}
;;

let copy_gv1 gv = 
	let tex = create_gv_texture "gv_texture2" gv.dim_x gv.dim_y gv.dim_z None in
	let textures = {Render_texture.tex_list = [|tex|]; Render_texture.depth_tex_list = [||]} in
	let fbo = Render_texture.create textures None in 
	{ dim_x = gv.dim_x; 
	  dim_y = gv.dim_y; 
	  dim_z = gv.dim_z;
	  cell_size = gv.cell_size;
	  bbox = gv.bbox; 
	  tex; fbo;}
;;


let create_blockers_from_geometry_buffer gv depth_normal_buffer vpls =
	FBO.glBindFrameBuffer FBO.GL_FRAMEBUFFER gv.fbo.Render_texture.id;
	let shader = Glsl_shader.Resource_Map.find "inject_blockers2" (!Glsl_shader.shader_list) in
	let program = shader.Glsl_shader.program in
	GL.glUseProgram program;
	
	let Texture.Texture_2D (blocker_depth_normal_base, blocker_depth_normal_params) = Texture.Resource_Map.find "blocker_depth_normal" (!Texture.texture_list) in

	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glBindTexture blocker_depth_normal_base.Texture.target blocker_depth_normal_base.Texture.tex_id;

	let loc = GL.glGetUniformLocation program "depth_normal_tex" in
	if loc = -1 then raise (Failure "load depth_normal_tex failure.");
	GL.glUniform1i loc 0;

	let loc = GL.glGetUniformLocation program "width" in
	if loc = -1 then raise (Failure "load width failure.");
	GL.glUniform1f loc (float depth_normal_buffer.Depth_normal_buffer.blocker_buffer_width);

	let loc = GL.glGetUniformLocation program "height" in
	if loc = -1 then raise (Failure "load height failure.");
	GL.glUniform1f loc (float depth_normal_buffer.Depth_normal_buffer.blocker_buffer_height);

	let loc = GL.glGetUniformLocation program "near_far_plane" in
	if loc = -1 then raise (Failure "load depth_normal_tex failure.");
	let Vector.Vec2 (x, y) = depth_normal_buffer.Depth_normal_buffer.near_far_plane in
	GL.glUniform2f loc x y;

	let loc = GL.glGetUniformLocation program "inv_proj" in
	if loc = -1 then raise (Failure "load inv_proj failure.");
	let Vector.Vec2 (x, y) = depth_normal_buffer.Depth_normal_buffer.inv_proj in
	GL.glUniform2f loc x y;

	let loc = GL.glGetUniformLocation program "view_to_grid_mat" in
	if loc = -1 then raise (Failure "load view_to_grid_mat failure.");
	GL.glUniformMatrix4f loc false depth_normal_buffer.Depth_normal_buffer.from_view_to_grid;

	let loc = GL.glGetUniformLocation program "grid_orig" in
	if loc = -1 then raise (Failure "load grid_orig failure.");
	let Vector.Vec3 (x, y, z) = gv.bbox.Bounding_box.min in
	GL.glUniform3f loc x y z;

	let Vector.Vec3 (x, y, z) = Bounding_box.calc_dim gv.bbox in
	let loc = GL.glGetUniformLocation program "grid_size" in
	if loc = -1 then raise (Failure "load grid_size failure.");
	GL.glUniform3f loc x y z;
	
	let Vector.Vec2 (x, y) = gv.cell_size in
	let loc = GL.glGetUniformLocation program "cell_area" in
	if loc = -1 then raise (Failure "load grid_size failure.");
	GL.glUniform1f loc (x *. y);

	let num_vpl = depth_normal_buffer.Depth_normal_buffer.blocker_buffer_width * depth_normal_buffer.Depth_normal_buffer.blocker_buffer_height in
	Printf.printf "number of vpls: %d\n" num_vpl; 
	GL.glClearColor 0.0 0.0 0.0 0.0;
	GL.glClear [GL.GL_COLOR_BUFFER_BIT];
	GL.glViewport 0 0 gv.dim_x gv.dim_y;
	GL.glEnable GL.GL_BLEND;
	GL.glBlendFunc GL.Sfactor.GL_ONE GL.Dfactor.GL_ONE;
	
	let vb_info = GL_buffer.Resource_Map.find "vpls" (!GL_buffer.buffer_list) in
	(* bind *)
	GL_buffer.bind_vertex_buffer_with_shader vb_info shader;
	Glex.glDrawArraysInstanced GL.GL_POINTS 0 1 num_vpl;
	GL_buffer.unbind_vertex_buffer_with_shader vb_info;

	GL.glDisable GL.GL_BLEND;
	GL.glActiveTexture GL.GL_TEXTURE0;	
	GL.glUnbindTexture blocker_depth_normal_base.Texture.target;

	FBO.glUnBindFrameBuffer FBO.GL_FRAMEBUFFER;
	injected := true
;;



let create_blockers_from_rsm gv rsm vpls =
	FBO.glBindFrameBuffer FBO.GL_FRAMEBUFFER gv.fbo.Render_texture.id;
	let shader = Glsl_shader.Resource_Map.find "inject_blocker" (!Glsl_shader.shader_list) in
	let program = shader.Glsl_shader.program in
	GL.glUseProgram program;
	
	let Texture.Texture_2D (depth_tex_base, depth_tex_params) = Texture.Resource_Map.find "rsm_depth_tex" (!Texture.texture_list) in

	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glBindTexture depth_tex_base.Texture.target depth_tex_base.Texture.tex_id;

	let Texture.Texture_2D (normal_tex_base, normal_tex_params) = Texture.Resource_Map.find "rsm_normal_tex" (!Texture.texture_list) in

	GL.glActiveTexture GL.GL_TEXTURE1;
	GL.glBindTexture normal_tex_base.Texture.target normal_tex_base.Texture.tex_id;

	let loc = GL.glGetUniformLocation program "width" in
	if loc = -1 then raise (Failure "load width failure.");
	GL.glUniform1f loc (float rsm.Rsm.width);

	let loc = GL.glGetUniformLocation program "height" in
	if loc = -1 then raise (Failure "load height failure.");
	GL.glUniform1f loc (float rsm.Rsm.height);

	let loc = GL.glGetUniformLocation program "cell_size_z" in
	if loc = -1 then raise (Failure "load cell_size_z failure.");
	let Vector.Vec3 (x, y, z) = gv.cell_size in
	GL.glUniform1f loc z;

	let loc = GL.glGetUniformLocation program "depth_tex" in
	if loc = -1 then raise (Failure "load inv_proj failure.");
	GL.glUniform1i loc 0;

	let loc = GL.glGetUniformLocation program "normal_tex" in
	if loc = -1 then raise (Failure "load normslz-tex failure.");
	GL.glUniform1i loc 1;

	let x = (float (gv.dim_x - 1))/.(float gv.dim_x) in
	let y = (float (gv.dim_y - 1))/.(float gv.dim_y) in
	let z = 0.5 /. (float gv.dim_x) in
	let k = 0.5 /. (float gv.dim_y) in
	

	let loc = GL.glGetUniformLocation program "proj_rsm_to_gv_grid" in
	if loc = -1 then raise (Failure "load grid_orig failure.");
	GL.glUniform4f loc x y z k;

	let point_weight = let tcells = float ((gv.dim_x - 1) * (gv.dim_y - 1)) and t = float (rsm.Rsm.width * rsm.Rsm.height) in tcells/.t in
	let loc = GL.glGetUniformLocation program "point_weight" in
	if loc = -1 then raise (Failure "load point_weight failure.");
	GL.glUniform1f loc point_weight;
	
	let Vector.Vec2 (x, y) = gv.cell_size in
	let loc = GL.glGetUniformLocation program "cell_area" in
	if loc = -1 then raise (Failure "load grid_size failure.");
	GL.glUniform1f loc (x *. y);

	let num_vpl = rsm.Rsm.width * rsm.Rsm.height in
	Printf.printf "number of vpls: %d\n" num_vpl; 
	GL.glClearColor 0.0 0.0 0.0 0.0;
	GL.glClear [GL.GL_COLOR_BUFFER_BIT];
	GL.glViewport 0 0 gv.dim_x gv.dim_y;
	GL.glEnable GL.GL_BLEND;
	GL.glBlendFunc GL.Sfactor.GL_ONE GL.Dfactor.GL_ONE;
	
	let vb_info = GL_buffer.Resource_Map.find "vpls" (!GL_buffer.buffer_list) in
	(* bind *)
	GL_buffer.bind_vertex_buffer_with_shader vb_info shader;
	Glex.glDrawArraysInstanced GL.GL_POINTS 0 1 num_vpl;
	GL_buffer.unbind_vertex_buffer_with_shader vb_info;

	GL.glDisable GL.GL_BLEND;
	GL.glActiveTexture GL.GL_TEXTURE0;	
	GL.glUnbindTexture depth_tex_base.Texture.target;

	GL.glActiveTexture GL.GL_TEXTURE1;	
	GL.glUnbindTexture normal_tex_base.Texture.target;

	FBO.glUnBindFrameBuffer FBO.GL_FRAMEBUFFER;
	injected := true
;;

let select_blockers dest_gv rsm_gv sisible_surface_gv slices = 
	FBO.glBindFrameBuffer FBO.GL_FRAMEBUFFER dest_gv.fbo.Render_texture.id;
	let shader = Glsl_shader.Resource_Map.find "select_gv" (!Glsl_shader.shader_list) in
	let program = shader.Glsl_shader.program in
	GL.glUseProgram program;
	
	let (rsm_gv_base, rsm_gv_params) = match Texture.Resource_Map.find "gv_texture0" (!Texture.texture_list) with 
							   Texture.Texture_3D (base, params) -> (base, params) 
							|  _ -> raise (Failure "gv_texture0 should be 3-dimension texture.")
	in
	let (visible_surface_gv_base, visible_surface_gv_params) = match Texture.Resource_Map.find "gv_texture1" (!Texture.texture_list) with
									  Texture.Texture_3D (base, params) -> (base, params)
									| _ -> raise (Failure "gv_texture1 should be 3-dimension texture.")
	in
	
	let loc = GL.glGetUniformLocation program "grid_depth" in
	if loc = -1 then raise (Failure "load grid_depth failure.");
	GL.glUniform1f loc (float dest_gv.dim_z);

	let Vector.Vec3 (x, y, z) = Vector.Vec3 (1./.(float dest_gv.dim_x), 1./.(float dest_gv.dim_y), 1./.(float dest_gv.dim_z)) in
	let loc = GL.glGetUniformLocation program "half_texel_size" in
	if loc = -1 then raise (Failure "load half_texel_size failure.");
	GL.glUniform1f loc (z *. 0.5);

	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glBindTexture rsm_gv_base.Texture.target rsm_gv_base.Texture.tex_id;

	GL.glActiveTexture GL.GL_TEXTURE1;
	GL.glBindTexture visible_surface_gv_base.Texture.target visible_surface_gv_base.Texture.tex_id;
	
	let loc = GL.glGetUniformLocation program "gv_from_rsm" in
	if loc = -1 then raise (Failure "load gv_from_rsm failure.");
	GL.glUniform1i loc 0;
	
	let loc = GL.glGetUniformLocation program "gv_from_visible_surface" in
	if loc = -1 then raise (Failure "load gv_from_visible_surface failure.");
	GL.glUniform1i loc 1;

	GL_buffer.bind_vertex_buffer_with_shader slices shader;
	Glex.glDrawArraysInstanced GL.GL_TRIANGLES 0 6 dest_gv.dim_z;
	GL_buffer.unbind_vertex_buffer_with_shader slices;

	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glUnbindTexture rsm_gv_base.Texture.target;

	GL.glActiveTexture GL.GL_TEXTURE1;
	GL.glUnbindTexture visible_surface_gv_base.Texture.target;

	FBO.glUnBindFrameBuffer FBO.GL_FRAMEBUFFER;

	injected := true;
;;
