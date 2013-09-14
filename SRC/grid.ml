(* create light volume *)
open GL
open FBO
open Glsl_shader
open Texture
open Render_texture

type t = {
		mutable red_tex     : GL.texture_id array;
		mutable green_tex   : GL.texture_id array;
		mutable blue_tex    : GL.texture_id array;
		mutable fbo_grid    : FBO.fbo_id array;
		mutable vpls        : GL_buffer.vertex_buffer;
		mutable slices      : GL_buffer.vertex_buffer;
		mutable _light      : Directional_light.light;		
		mutable iteration   : int;
		mutable dim_x       : int;
		mutable dim_y       : int;
		mutable dim_z       : int;
		mutable cell_size   : Vector.vec;
		mutable grid_origin : Vector.vec;
		mutable gv0         : Geometry_volume.t;
		mutable gv1         : Geometry_volume.t;
		mutable gv2         : Geometry_volume.t;
	 }

let create_shader () = 
	let propagate_vertex_shader_file = "../shader/propagate.vp" in
	let propagate_fragment_shader_file = "../shader/propagate.fp" in
	let propagate_geometry_shader_file = "../shader/propagate.gp" in
	let geometry_params = {Glsl_shader.input_type = 4; Glsl_shader.output_type = 5; Glsl_shader.vertices_out = 3} in
	let shader_info = Glsl_shader.create propagate_vertex_shader_file propagate_fragment_shader_file (Some propagate_geometry_shader_file) (Some geometry_params) None in
	Glsl_shader.insert_shader_list "propagate" shader_info;

	let propagate_vertex_shader_file = "../shader/propagate.vp" in
	let propagate_fragment_shader_file = "../shader/propagate.fp" in
	let propagate_geometry_shader_file = "../shader/propagate.gp" in
	let geometry_params = {Glsl_shader.input_type = 4; Glsl_shader.output_type = 5; Glsl_shader.vertices_out = 3} in
	let shader_info = Glsl_shader.create propagate_vertex_shader_file propagate_fragment_shader_file (Some propagate_geometry_shader_file) (Some geometry_params) (Some "#define NO_BLOCKING\n") in
	Glsl_shader.insert_shader_list "propagate_no_blocking" shader_info;
	
	let accumulate_vertex_shader_file = "../shader/propagate.vp" in
	let accumulate_fragment_shader_file = "../shader/accumulate.fp" in
	let accumulate_geometry_shader_file = "../shader/propagate.gp" in
	let geometry_params = {Glsl_shader.input_type = 4; Glsl_shader.output_type = 5; Glsl_shader.vertices_out = 3} in
	let shader_info = Glsl_shader.create accumulate_vertex_shader_file accumulate_fragment_shader_file (Some accumulate_geometry_shader_file) (Some geometry_params) None in
	Glsl_shader.insert_shader_list "accumulate" shader_info;
	
	let inject_vertex_shader_file = "../shader/inject.vp" in
	let inject_fragment_shader_file = "../shader/inject.fp" in
	let inject_geometry_shader_file = "../shader/inject.gp" in
	let geometry_params = {Glsl_shader.input_type = 0; Glsl_shader.output_type = 0; Glsl_shader.vertices_out = 1} in
	let shader_info = Glsl_shader.create inject_vertex_shader_file inject_fragment_shader_file (Some inject_geometry_shader_file) (Some geometry_params) None in
	Glsl_shader.insert_shader_list "inject" shader_info
;;

let create_grid_textures () =
	let grid_tex_params = Texture.init_param_3d in
	let red_tex = Array.mapi (fun i x  ->   let base = {Texture.tex_id = GL.glGenTexture (); Texture.target = GL.BindTex.GL_TEXTURE_3D; Texture.name = let tmp = Printf.sprintf "light_intensity_red_%d" i in tmp} in 
						let params = {grid_tex_params with Texture.Texture_Params_3D.min_filter = GL.Min.GL_NEAREST; 
								 Texture.Texture_Params_3D.mag_filter = GL.Mag.GL_NEAREST;
		    		         			 Texture.Texture_Params_3D.source_format = GL.GL_RGBA;
			        	 			 Texture.Texture_Params_3D.internal_format = Glex.GL_RGBA16F;
					 			 Texture.Texture_Params_3D.n_type = GL.GL_FLOAT}
						in	
						Texture.create_texture_3d base params 16 16 16 None;
						base.Texture.tex_id
				 ) [|0;0;0|] in

	let green_tex = Array.mapi (fun i x ->  let base = {Texture.tex_id = GL.glGenTexture (); Texture.target = GL.BindTex.GL_TEXTURE_3D; Texture.name = let tmp = Printf.sprintf "light_intensity_green_%d" i in tmp} in 
						let params = {grid_tex_params with Texture.Texture_Params_3D.min_filter = GL.Min.GL_NEAREST; 
								 Texture.Texture_Params_3D.mag_filter = GL.Mag.GL_NEAREST;
		    		         			 Texture.Texture_Params_3D.source_format = GL.GL_RGBA;
			        	 			 Texture.Texture_Params_3D.internal_format = Glex.GL_RGBA16F;
					 			 Texture.Texture_Params_3D.n_type = GL.GL_FLOAT}
						in	
						Texture.create_texture_3d base params 16 16 16 None;
						base.Texture.tex_id
				   ) [|0;0;0|] in

	let blue_tex = Array.mapi (fun i x  ->  let base = {Texture.tex_id = GL.glGenTexture (); Texture.target = GL.BindTex.GL_TEXTURE_3D; Texture.name = let tmp = Printf.sprintf "light_intensity_blue_%d" i in tmp} in 
						let params = {grid_tex_params with Texture.Texture_Params_3D.min_filter = GL.Min.GL_NEAREST; 
								 Texture.Texture_Params_3D.mag_filter = GL.Mag.GL_NEAREST;
		    		         			 Texture.Texture_Params_3D.source_format = GL.GL_RGBA;
			        	 			 Texture.Texture_Params_3D.internal_format = Glex.GL_RGBA16F;
					 			 Texture.Texture_Params_3D.n_type = GL.GL_FLOAT}
						in	
						Texture.create_texture_3d base params 16 16 16 None;
						base.Texture.tex_id
				  ) [|0;0;0|] in
	(red_tex, green_tex, blue_tex)
;;

(* parameters:                                                               *)
(* 	red_tex array, green_tex array, blue_tex array                       *)
(*                                                                           *)
(* return:                                                                   *)
(*	fbo_id array                                                         *)
let create_grid_rt red_tex green_tex blue_tex = 
	Array.map (fun (red,green,blue) -> let textures = {Render_texture.tex_list=[|red;green;blue|]; Render_texture.depth_tex_list=[||]} in
					     Render_texture.create textures None
	) 
	[|(red_tex.(0),green_tex.(0),blue_tex.(0)); (red_tex.(1),green_tex.(1),blue_tex.(1)); (red_tex.(2),green_tex.(2),blue_tex.(2))|] 
;;


let create_slices () = 
	let pos_array = [| 0.0; 0.0; 1.0; 0.0; 0.0; 1.0; 1.0; 0.0; 1.0; 1.0; 0.0; 1.0 |] in
	let decl_array = [|{GL_buffer.field = GL_buffer.VEC2F; GL_buffer.binding_name = "position"}|] in
	let vb_info = GL_buffer.create_vertex_buffer "slices" decl_array 6 VBO.GL_STATIC_DRAW in
	Printf.printf "%d\n" vb_info.GL_buffer.buffer_size;
	GL_buffer.insert_buffer_list "slices" vb_info;
	GL_buffer.bind_vertex_buffer vb_info.GL_buffer.buf;
	GL_buffer.set_vertex_buffer_data 0 vb_info.GL_buffer.buffer_size pos_array;
	vb_info
	(* unbind *)
;;

let create_vpls width height = 
	let decl_array = [|{GL_buffer.field = GL_buffer.VEC2F; GL_buffer.binding_name = "position"}|] in
	let vb_info = GL_buffer.create_vertex_buffer "vpls" decl_array 1 VBO.GL_STATIC_DRAW in
	Printf.printf "%d\n" vb_info.GL_buffer.buffer_size;
	GL_buffer.insert_buffer_list "vpls" vb_info;
	GL_buffer.bind_vertex_buffer vb_info.GL_buffer.buf;
	GL_buffer.set_vertex_buffer_data 0 vb_info.GL_buffer.buffer_size [|0.0; 0.0|];
	vb_info
	(* unbind *)
;;

let create bbox dim_x dim_y dim_z width height _light = 
	let iteration = 8 in
	let Vector.Vec3 (x, y, z) = Bounding_box.calc_dim bbox in
	let cell_size = Vector.Vec3 ( x/.(float dim_x) , y/.(float dim_y) , z/.(float dim_z)) in
	(*create_shader ();*)
	let (red_tex, green_tex, blue_tex) = create_grid_textures () in
	let fbo_grid = create_grid_rt red_tex green_tex blue_tex in
	let slices = create_slices () in
	let vpls = create_vpls width height in
	let gv0 = Geometry_volume.create cell_size (dim_x+1) (dim_y+1) (dim_z+1) in
	let gv1 = Geometry_volume.copy_gv0 gv0 in
	let gv2 = Geometry_volume.copy_gv1 gv1 in
	{
		red_tex; green_tex; blue_tex; fbo_grid; slices; vpls; _light; iteration;
		dim_x;dim_y;dim_z;
		cell_size;
		grid_origin = bbox.Bounding_box.min;
		gv0;gv1;gv2;
	}
;;
