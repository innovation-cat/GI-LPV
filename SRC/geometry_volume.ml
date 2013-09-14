type t = {
		mutable dim_x 			: int;
		mutable dim_y 			: int;
		mutable dim_z 			: int;
		mutable cell_size               : Vector.vec;
		mutable bbox 			: Bounding_box.bounding_box;
		mutable tex   			: GL.texture_id;
		mutable fbo   			: FBO.fbo_id;
	}

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
	let bbox = Bounding_box.create () in
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
