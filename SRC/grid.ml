(* create light volume *)
open GL
open FBO
open Glsl_shader
open Texture
open Render_texture

type t = {
		mutable red_tex      : Texture.texture array;
		mutable green_tex    : Texture.texture array;
		mutable blue_tex     : Texture.texture array;
		mutable fbo_grid     : Render_texture.t array;
		mutable vpls         : GL_buffer.vertex_buffer;
		mutable slices       : GL_buffer.vertex_buffer;
		mutable _light       : Directional_light.light;		
		mutable iteration    : int;
		mutable dim_x        : int;
		mutable dim_y        : int;
		mutable dim_z        : int;
		mutable cell_size    : Vector.vec;
		mutable grid_origin  : Vector.vec;
		mutable gv0          : Geometry_volume.t;
		mutable gv1          : Geometry_volume.t;
		mutable gv2          : Geometry_volume.t;
		mutable source_grid  : int;
		mutable dest_grid    : int;
		mutable light_volume : int;
		mutable grid_to_show : int;
		mutable iterations   : int;
	 }

let create_shader () = 
	let propagate_vertex_shader_file = "../shader/propagate.vp" in
	let propagate_fragment_shader_file = "../shader/propagate.fp" in
	let propagate_geometry_shader_file = "../shader/propagate.gp" in
	let geometry_params = {Glsl_shader.input_type = 4; Glsl_shader.output_type = 5; Glsl_shader.vertices_out = 3} in
	let shader_info = Glsl_shader.create propagate_vertex_shader_file propagate_fragment_shader_file (Some propagate_geometry_shader_file) (Some geometry_params) None in
	Glsl_shader.insert_shader_list "propagate" shader_info;
(*
	let propagate_vertex_shader_file = "../shader/propagate_no_blocking.vp" in
	let propagate_fragment_shader_file = "../shader/propagate_no_blocking.fp" in
	let propagate_geometry_shader_file = "../shader/propagate_no_blocking.gp" in
	let geometry_params = {Glsl_shader.input_type = 4; Glsl_shader.output_type = 5; Glsl_shader.vertices_out = 3} in
	let shader_info = Glsl_shader.create propagate_vertex_shader_file propagate_fragment_shader_file (Some propagate_geometry_shader_file) (Some geometry_params) None in
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
*)
;;

let grid_tex_params = {Texture.init_param_3d with Texture.Texture_Params_3D.min_filter = GL.Min.GL_NEAREST; 
				   		  Texture.Texture_Params_3D.mag_filter = GL.Mag.GL_NEAREST;
		    		   		  Texture.Texture_Params_3D.source_format = GL.GL_RGBA;
			           		  Texture.Texture_Params_3D.internal_format = Glex.GL_RGBA16F;
				   		  Texture.Texture_Params_3D.n_type = GL.GL_FLOAT}
;;

let create_grid_textures () =
	let red_tex = Array.mapi (fun i x  ->   let base = {Texture.tex_id = GL.glGenTexture (); Texture.target = GL.BindTex.GL_TEXTURE_3D; Texture.name = let tmp = Printf.sprintf "light_intensity_red_%d" i in tmp} in 	
						Texture.create_texture_3d base grid_tex_params 16 16 16 None;
						Texture.Texture_3D (base, grid_tex_params)
				 ) [|0;0;0|] in

	let green_tex = Array.mapi (fun i x ->  let base = {Texture.tex_id = GL.glGenTexture (); Texture.target = GL.BindTex.GL_TEXTURE_3D; Texture.name = let tmp = Printf.sprintf "light_intensity_green_%d" i in tmp} in 
						Texture.create_texture_3d base grid_tex_params 16 16 16 None;
						Texture.Texture_3D (base, grid_tex_params)
				   ) [|0;0;0|] in

	let blue_tex = Array.mapi (fun i x  ->  let base = {Texture.tex_id = GL.glGenTexture (); Texture.target = GL.BindTex.GL_TEXTURE_3D; Texture.name = let tmp = Printf.sprintf "light_intensity_blue_%d" i in tmp} in 
						Texture.create_texture_3d base grid_tex_params 16 16 16 None;
						Texture.Texture_3D (base, grid_tex_params)
				  ) [|0;0;0|] in
	(red_tex, green_tex, blue_tex)
;;

(* parameters:                                                               *)
(* 	red_tex array, green_tex array, blue_tex array                       *)
(*                                                                           *)
(* return:                                                                   *)
(*	fbo_id array                                                         *)
let create_grid_rt red_tex green_tex blue_tex = 
	let Texture.Texture_3D (red_tex0_base, red_tex0_params) = red_tex.(0) in
	let Texture.Texture_3D (red_tex1_base, red_tex1_params) = red_tex.(1) in
	let Texture.Texture_3D (red_tex2_base, red_tex2_params) = red_tex.(2) in
	let Texture.Texture_3D (green_tex0_base, green_tex0_params) = green_tex.(0) in
	let Texture.Texture_3D (green_tex1_base, green_tex1_params) = green_tex.(1) in
	let Texture.Texture_3D (green_tex2_base, green_tex2_params) = green_tex.(2) in
	let Texture.Texture_3D (blue_tex0_base, blue_tex0_params) = blue_tex.(0) in
	let Texture.Texture_3D (blue_tex1_base, blue_tex1_params) = blue_tex.(1) in
	let Texture.Texture_3D (blue_tex2_base, blue_tex2_params) = blue_tex.(2) in
	Array.map (fun (red,green,blue) -> let textures = {Render_texture.tex_list=[|red.Texture.tex_id; green.Texture.tex_id; blue.Texture.tex_id|]; 
							   Render_texture.depth_tex_list=[||]} 
					   in
					   Render_texture.create textures None
	) 
	[|(red_tex0_base, green_tex0_base, blue_tex0_base); 
	  (red_tex1_base, green_tex1_base, blue_tex1_base); 
	  (red_tex2_base, green_tex2_base, blue_tex2_base)|] 
;;


let create_slices () = 
	let pos_array = [| 0.0; 0.0; 1.0; 0.0; 0.0; 1.0; 1.0; 0.0; 1.0; 1.0; 0.0; 1.0 |] in
	let decl_array = [|{GL_buffer.field = GL_buffer.VEC2F; GL_buffer.binding_name = "position"}|] in
	let vb_info = GL_buffer.create_vertex_buffer "slices" decl_array 6 VBO.GL_STATIC_DRAW in
	Printf.printf "%d\n" vb_info.GL_buffer.buffer_size;
	GL_buffer.insert_buffer_list "slices" vb_info;
	GL_buffer.bind_vertex_buffer vb_info;
	GL_buffer.set_vertex_buffer_data 0 vb_info.GL_buffer.buffer_size pos_array;
	vb_info
	(* unbind *)
;;

let create_vpls width height = 
	let decl_array = [|{GL_buffer.field = GL_buffer.VEC2F; GL_buffer.binding_name = "position"}|] in
	let vb_info = GL_buffer.create_vertex_buffer "vpls" decl_array 1 VBO.GL_STATIC_DRAW in
	Printf.printf "%d\n" vb_info.GL_buffer.buffer_size;
	GL_buffer.insert_buffer_list "vpls" vb_info;
	GL_buffer.bind_vertex_buffer vb_info;
	GL_buffer.set_vertex_buffer_data 0 vb_info.GL_buffer.buffer_size [|0.0; 0.0|];
	vb_info
	(* unbind *)
;;

let create bbox dim_x dim_y dim_z width height _light = 
	let iteration = 8 in
	let Vector.Vec3 (x, y, z) = Bounding_box.calc_dim bbox in
	let cell_size = Vector.Vec3 ( x/.(float dim_x) , y/.(float dim_y) , z/.(float dim_z)) in
	create_shader ();
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
		source_grid = 0;
		dest_grid = 1;
		light_volume = 2;
		grid_to_show = 0;
		iterations = 8;
	}
;;


let inject_from_depth_normal_buffer grid _depth_normal_buffer = 
	Geometry_volume.create_blockers_from_geometry_buffer grid.gv0 _depth_normal_buffer grid.vpls
;;

let inject_from_rsm grid _rsm = 
	Geometry_volume.create_blockers_from_rsm grid.gv1 _rsm grid.vpls
;;

let select_grid grid = 
	Geometry_volume.select_blockers grid.gv2 grid.gv0 grid.gv1 grid.slices
;;

let propagate grid gv_texture first_iteration = 
	let shader = if first_iteration = true then 
			Glsl_shader.Resource_Map.find "propagate_no_blocking" (!Glsl_shader.shader_list)
		     else 
			Glsl_shader.Resource_Map.find "propagate" (!Glsl_shader.shader_list)
	in
	let program = shader.Glsl_shader.program in
	GL.glUseProgram program;
	
	let loc = GL.glGetUniformLocation program "grid_depth" in
	if loc = -1 then raise (Failure "load grid_depth failure.");
	GL.glUniform1f loc (float grid.dim_z);

	let Vector.Vec3 (x, y, z) = Vector.Vec3 (1.0/.(float grid.dim_x), 1.0/.(float grid.dim_y), 1.0/.(float grid.dim_z)) in
	let loc = GL.glGetUniformLocation program "inv_grid_size" in
	if loc = -1 then raise (Failure "load inv_grid_size failure.");
	GL.glUniform3f loc x y z;

	let loc = GL.glGetUniformLocation program "half_texel_size" in
	if loc = -1 then raise (Failure "load half_texel_size failure.");
	GL.glUniform1f loc (z *. 0.5);
	
	let loc = GL.glGetUniformLocation program "coeffs_red" in
	if loc = -1 then raise (Failure "load coeffs_red failure.");
	GL.glUniform1i loc 0;
	
	let loc = GL.glGetUniformLocation program "coeffs_green" in
	if loc = -1 then raise (Failure "load coeffs_green failure.");
	GL.glUniform1i loc 1;

	let loc = GL.glGetUniformLocation program "coeffs_blue" in
	if loc = -1 then raise (Failure "load coeffs_blue failure.");
	GL.glUniform1i loc 2;

	if first_iteration = false then begin
		let loc = GL.glGetUniformLocation program "geometry_volume" in
		if loc = -1 then raise (Failure "load geometry_volume failure.");
		GL.glUniform1i loc 3;
	
		let proj_grid_to_gv = [|(float grid.dim_x)/.(float (grid.dim_x + 1));
					(float grid.dim_y)/.(float (grid.dim_y + 1));
					(float grid.dim_z)/.(float (grid.dim_z + 1));
					0.5 /.(float (grid.dim_x + 1));
					0.5 /.(float (grid.dim_y + 1));
					0.5 /.(float (grid.dim_z + 1)); |]
		in
		let loc = GL.glGetUniformLocation program "proj_grid_to_gv" in
		if loc = -1 then raise (Failure "load proj_grid_to_gv failure.");
		GL.glUniform3fv loc 2 proj_grid_to_gv;
	end;

	FBO.glBindFrameBuffer FBO.GL_FRAMEBUFFER grid.fbo_grid.(grid.dest_grid).Render_texture.id;
	
	let (light_intensity_red_base, light_intensity_red_params) = match grid.red_tex.(grid.source_grid) with
									  Texture.Texture_3D (base, params) -> (base, params) 
									|  _ -> raise (Failure "red_tex should be 3-dimension texture.")
	in
	let (light_intensity_green_base, light_intensity_green_params) = match grid.green_tex.(grid.source_grid) with
									  Texture.Texture_3D (base, params) -> (base, params) 
									|  _ -> raise (Failure "green_tex should be 3-dimension texture.")
	in
	let (light_intensity_blue_base, light_intensity_blue_params) = match grid.blue_tex.(grid.source_grid) with
									  Texture.Texture_3D (base, params) -> (base, params) 
									|  _ -> raise (Failure "blue_tex should be 3-dimension texture.")
	in
	
	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glBindTexture light_intensity_red_base.Texture.target light_intensity_red_base.Texture.tex_id;

	GL.glActiveTexture GL.GL_TEXTURE1;
	GL.glBindTexture light_intensity_green_base.Texture.target light_intensity_green_base.Texture.tex_id;

	GL.glActiveTexture GL.GL_TEXTURE2;
	GL.glBindTexture light_intensity_blue_base.Texture.target light_intensity_blue_base.Texture.tex_id;

	if first_iteration = false then begin
		let (gv_texture_base, gv_texture_params) = match gv_texture with	
								  Texture.Texture_3D (base, params) -> (base, params) 
								|  _ -> raise (Failure "gv_texture should be 3-dimension texture.")
		in
		GL.glActiveTexture GL.GL_TEXTURE3;
		GL.glBindTexture gv_texture_base.Texture.target gv_texture_base.Texture.tex_id;
	end;
	

	let propagate_shader = Glsl_shader.Resource_Map.find "propagate" (!Glsl_shader.shader_list) in
	GL_buffer.bind_vertex_buffer_with_shader grid.slices propagate_shader;
	Glex.glDrawArraysInstanced GL.GL_POINTS 0 6 grid.dim_z;
	GL_buffer.unbind_vertex_buffer_with_shader grid.slices;

	if first_iteration = false then begin
		let (gv_texture_base, gv_texture_params) = match gv_texture with	
								  Texture.Texture_3D (base, params) -> (base, params) 
								|  _ -> raise (Failure "gv_texture should be 3-dimension texture.")
		in
		GL.glActiveTexture GL.GL_TEXTURE3;
		GL.glUnbindTexture gv_texture_base.Texture.target;
	end;
	
	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glUnbindTexture light_intensity_red_base.Texture.target;

	GL.glActiveTexture GL.GL_TEXTURE1;
	GL.glUnbindTexture light_intensity_green_base.Texture.target;

	GL.glActiveTexture GL.GL_TEXTURE2;
	GL.glUnbindTexture light_intensity_blue_base.Texture.target;

	FBO.glUnBindFrameBuffer FBO.GL_FRAMEBUFFER
;;


let accumulate_in_light_volume grid source dest =
	(* setup_accumulate_shader *)
	let shader = Glsl_shader.Resource_Map.find "accumulate" (!Glsl_shader.shader_list) in
	let program = shader.Glsl_shader.program in
	GL.glUseProgram program;
	
	let loc = GL.glGetUniformLocation program "grid_depth" in
	if loc = -1 then raise (Failure "load grid_depth failure.");
	GL.glUniform1f loc (float grid.dim_z);

	let Vector.Vec3 (x, y, z) = Vector.Vec3 (1.0/.(float grid.dim_x), 1.0/.(float grid.dim_y), 1.0/.(float grid.dim_z)) in
	let loc = GL.glGetUniformLocation program "half_texel_size" in
	if loc = -1 then raise (Failure "load half_texel_size failure.");
	GL.glUniform1f loc (z *. 0.5);
	
	let loc = GL.glGetUniformLocation program "spectral_coeffs_r" in
	if loc = -1 then raise (Failure "load spectral_coeffs_r failure.");
	GL.glUniform1i loc 0;
	
	let loc = GL.glGetUniformLocation program "spectral_coeffs_g" in
	if loc = -1 then raise (Failure "load spectral_coeffs_g failure.");
	GL.glUniform1i loc 1;

	let loc = GL.glGetUniformLocation program "spectral_coeffs_b" in
	if loc = -1 then raise (Failure "load spectral_coeffs_b failure.");
	GL.glUniform1i loc 2;

	(* *)
	
	
	let (light_intensity_red_base, light_intensity_red_params) = match grid.red_tex.(source) with
									  Texture.Texture_3D (base, params) -> (base, params) 
									|  _ -> raise (Failure "red_tex should be 3-dimension texture.")
	in
	let (light_intensity_green_base, light_intensity_green_params) = match grid.green_tex.(source) with
									  Texture.Texture_3D (base, params) -> (base, params) 
									|  _ -> raise (Failure "green_tex should be 3-dimension texture.")
	in
	let (light_intensity_blue_base, light_intensity_blue_params) = match grid.blue_tex.(source) with
									  Texture.Texture_3D (base, params) -> (base, params) 
									|  _ -> raise (Failure "blue_tex should be 3-dimension texture.")
	in
	
	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glBindTexture light_intensity_red_base.Texture.target light_intensity_red_base.Texture.tex_id;

	GL.glActiveTexture GL.GL_TEXTURE1;
	GL.glBindTexture light_intensity_green_base.Texture.target light_intensity_green_base.Texture.tex_id;

	GL.glActiveTexture GL.GL_TEXTURE2;
	GL.glBindTexture light_intensity_blue_base.Texture.target light_intensity_blue_base.Texture.tex_id;

	FBO.glBindFrameBuffer FBO.GL_FRAMEBUFFER grid.fbo_grid.(dest).Render_texture.id;

	GL.glEnable GL.GL_BLEND;
	GL.glBlendFunc GL.Sfactor.GL_ONE GL.Dfactor.GL_ONE;

	GL_buffer.bind_vertex_buffer_with_shader grid.slices shader;
	Glex.glDrawArraysInstanced GL.GL_TRIANGLES 0 6 grid.dim_z;
	GL_buffer.unbind_vertex_buffer_with_shader grid.slices;

	GL.glDisable GL.GL_BLEND;
	
	FBO.glUnBindFrameBuffer FBO.GL_FRAMEBUFFER;
	
	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glUnbindTexture light_intensity_red_base.Texture.target;

	GL.glActiveTexture GL.GL_TEXTURE1;
	GL.glUnbindTexture light_intensity_green_base.Texture.target;

	GL.glActiveTexture GL.GL_TEXTURE2;
	GL.glUnbindTexture light_intensity_blue_base.Texture.target;
;;


let propagate_and_accumulate grid gv_texture =
	propagate grid gv_texture true;

	accumulate_in_light_volume grid grid.dest_grid grid.source_grid;

	let tmp = grid.light_volume in
	grid.light_volume <- grid.source_grid;
	grid.source_grid <- grid.dest_grid;
	grid.dest_grid <- tmp;
	
	let arr = Array.make (grid.iterations - 1) 0 in
	Array.iter (fun x  ->   propagate grid gv_texture false; 
			 	accumulate_in_light_volume grid grid.dest_grid grid.light_volume;
				(* swap source_grid and dest_grid *)			 	
				let tmp = grid.source_grid in
			 	grid.source_grid <- grid.dest_grid;
				grid.dest_grid <- tmp ) arr
;;
	 

let inject_vpls grid rsm gv_texture = 
	let shader = Glsl_shader.Resource_Map.find "inject" (!Glsl_shader.shader_list) in
	let program = shader.Glsl_shader.program in
	GL.glUseProgram program;
	
	let loc = GL.glGetUniformLocation program "width" in
	if loc = -1 then raise (Failure "load width failure.");
	GL.glUniform1f loc (float rsm.Rsm.width);

	let loc = GL.glGetUniformLocation program "height" in
	if loc = -1 then raise (Failure "load height failure.");
	GL.glUniform1f loc (float rsm.Rsm.height);

	let Vector.Vec3 (x, y, z) = grid._light.Directional_light.grid_space.Directional_light.light_dir in
	let loc = GL.glGetUniformLocation program "light_dir" in
	if loc = -1 then raise (Failure "load light_dir failure.");
	GL.glUniform3f loc x y z;
	
	let Vector.Vec3 (x, y, z) = Bounding_box.calc_dim grid._light.Directional_light.grid_bbox in
	let loc = GL.glGetUniformLocation program "grid_size" in
	if loc = -1 then raise (Failure "load grid_size failure.");
	GL.glUniform2f loc x y;

	let Vector.Vec3 (cell_x, cell_y, cell_z) = Vector.Vec3 (x/.(float grid.dim_x), y/.(float grid.dim_y), z/.(float grid.dim_z)) in
	let loc = GL.glGetUniformLocation program "cell_size" in
	if loc = -1 then raise (Failure "load cell_size failure.");
	GL.glUniform3f loc cell_x cell_y cell_z;
	
	let loc = GL.glGetUniformLocation program "color_tex" in
	if loc = -1 then raise (Failure "load color_tex failure.");
	GL.glUniform1i loc 0;
	
	let loc = GL.glGetUniformLocation program "depth_tex" in
	if loc = -1 then raise (Failure "load depth_tex failure.");
	GL.glUniform1i loc 1;

	let loc = GL.glGetUniformLocation program "normal_tex" in
	if loc = -1 then raise (Failure "load normal_tex failure.");
	GL.glUniform1i loc 2;

	let point_weight = let tcells = float (grid.dim_x * grid.dim_y) and t = float (rsm.Rsm.width * rsm.Rsm.height) in tcells/.t in
	let loc = GL.glGetUniformLocation program "point_weight" in
	if loc = -1 then raise (Failure "load point_weight failure.");
	GL.glUniform1f loc point_weight;

	let (rsm_color_tex_base, rsm_color_tex_params) = match Texture.Resource_Map.find "rsm_color_tex" (!Texture.texture_list) with 
							   Texture.Texture_2D (base, params) -> (base, params) 
							|  _ -> raise (Failure "rsm_color_tex should be 2-dimension texture.")
	in
	let (rsm_depth_tex_base, rsm_depth_tex_params) = match Texture.Resource_Map.find "rsm_depth_tex" (!Texture.texture_list) with 
							   Texture.Texture_2D (base, params) -> (base, params) 
							|  _ -> raise (Failure "rsm_depth_tex should be 2-dimension texture.")
	in
	let (rsm_normal_tex_base, rsm_normal_tex_params) = match Texture.Resource_Map.find "rsm_normal_tex" (!Texture.texture_list) with 
							   Texture.Texture_2D (base, params) -> (base, params) 
							|  _ -> raise (Failure "rsm_normal_tex should be 2-dimension texture.")
	in
	
	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glBindTexture rsm_color_tex_base.Texture.target rsm_color_tex_base.Texture.tex_id;

	GL.glActiveTexture GL.GL_TEXTURE1;
	GL.glBindTexture rsm_depth_tex_base.Texture.target rsm_depth_tex_base.Texture.tex_id;

	GL.glActiveTexture GL.GL_TEXTURE2;
	GL.glBindTexture rsm_normal_tex_base.Texture.target rsm_normal_tex_base.Texture.tex_id;

	let num_vpls = rsm.Rsm.width * rsm.Rsm.height in

	FBO.glBindFrameBuffer FBO.GL_FRAMEBUFFER grid.fbo_grid.(grid.source_grid).Render_texture.id;

	GL.glClearColor 0.0 0.0 0.0 0.0;
	GL.glClear [GL.GL_COLOR_BUFFER_BIT];
	GL.glViewport 0 0 grid.dim_x grid.dim_y;
	GL.glEnable GL.GL_BLEND;
	GL.glBlendFunc GL.Sfactor.GL_ONE GL.Dfactor.GL_ONE;
	
	GL_buffer.bind_vertex_buffer_with_shader grid.vpls shader;
	Glex.glDrawArraysInstanced GL.GL_POINTS 0 1 num_vpls;
	GL_buffer.unbind_vertex_buffer_with_shader grid.vpls;

	GL.glDisable GL.GL_BLEND;

	FBO.glUnBindFrameBuffer FBO.GL_FRAMEBUFFER;

	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glUnbindTexture rsm_color_tex_base.Texture.target;

	GL.glActiveTexture GL.GL_TEXTURE1;
	GL.glUnbindTexture rsm_depth_tex_base.Texture.target;

	GL.glActiveTexture GL.GL_TEXTURE2;
	GL.glUnbindTexture rsm_normal_tex_base.Texture.target;

	grid.grid_to_show <- grid.source_grid;
	
	if grid.iterations > 0 then begin
		propagate_and_accumulate grid gv_texture;
		grid.grid_to_show <- grid.light_volume;
	end;
;;

let bind_light_volume_textures grid = 
	let params = {grid_tex_params with Texture.Texture_Params_3D.min_filter = GL.Min.GL_LINEAR; 
					   Texture.Texture_Params_3D.mag_filter = GL.Mag.GL_LINEAR;}
	in
	
	let (light_intensity_red_base, light_intensity_red_params) = match grid.red_tex.(grid.grid_to_show) with
									  Texture.Texture_3D (base, params) -> (base, params) 
									|  _ -> raise (Failure "red_tex should be 3-dimension texture.")
	in
	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glBindTexture light_intensity_red_base.Texture.target light_intensity_red_base.Texture.tex_id;
	Texture.update_texture_3d light_intensity_red_base params;

	let (light_intensity_green_base, light_intensity_green_params) = match grid.green_tex.(grid.grid_to_show) with
									  Texture.Texture_3D (base, params) -> (base, params) 
									|  _ -> raise (Failure "green_tex should be 3-dimension texture.")
	in
	GL.glActiveTexture GL.GL_TEXTURE1;
	GL.glBindTexture light_intensity_green_base.Texture.target light_intensity_green_base.Texture.tex_id;
	Texture.update_texture_3d light_intensity_green_base params;

	let (light_intensity_blue_base, light_intensity_blue_params) = match grid.blue_tex.(grid.grid_to_show) with
									  Texture.Texture_3D (base, params) -> (base, params) 
									|  _ -> raise (Failure "blue_tex should be 3-dimension texture.")
	in
	GL.glActiveTexture GL.GL_TEXTURE2;
	GL.glBindTexture light_intensity_blue_base.Texture.target light_intensity_blue_base.Texture.tex_id;
	Texture.update_texture_3d light_intensity_blue_base params;
;;

let unbind_light_volume_textures grid = 
	let (light_intensity_red_base, light_intensity_red_params) = match grid.red_tex.(grid.grid_to_show) with
									  Texture.Texture_3D (base, params) -> (base, params) 
									|  _ -> raise (Failure "red_tex should be 3-dimension texture.")
	in
	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glBindTexture light_intensity_red_base.Texture.target light_intensity_red_base.Texture.tex_id;
	Texture.update_texture_3d light_intensity_red_base grid_tex_params;
	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glUnbindTexture light_intensity_red_base.Texture.target;

	let (light_intensity_green_base, light_intensity_green_params) = match grid.green_tex.(grid.grid_to_show) with
									  Texture.Texture_3D (base, params) -> (base, params) 
									|  _ -> raise (Failure "green_tex should be 3-dimension texture.")
	in
	GL.glActiveTexture GL.GL_TEXTURE1;
	GL.glBindTexture light_intensity_green_base.Texture.target light_intensity_green_base.Texture.tex_id;
	Texture.update_texture_3d light_intensity_green_base grid_tex_params;
	GL.glActiveTexture GL.GL_TEXTURE1;
	GL.glUnbindTexture light_intensity_green_base.Texture.target;

	let (light_intensity_blue_base, light_intensity_blue_params) = match grid.blue_tex.(grid.grid_to_show) with
									  Texture.Texture_3D (base, params) -> (base, params) 
									|  _ -> raise (Failure "blue_tex should be 3-dimension texture.")
	in
	GL.glActiveTexture GL.GL_TEXTURE2;
	GL.glBindTexture light_intensity_blue_base.Texture.target light_intensity_blue_base.Texture.tex_id;
	Texture.update_texture_3d light_intensity_blue_base grid_tex_params;
	GL.glActiveTexture GL.GL_TEXTURE2;
	GL.glUnbindTexture light_intensity_blue_base.Texture.target;
;;
