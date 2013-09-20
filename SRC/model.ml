open GL

type t = {
		mutable bbox          : Bounding_box.bounding_box;
		mutable vb_info       : GL_buffer.vertex_buffer;
		mutable wall_texture  : GL.texture_id;
		mutable floor_texture : GL.texture_id;
	 }

let (|>) x f = f x;;

let material_color = ref (Vector.Vec4 (0.0, 0.0, 0.0, 1.0))

let oc = open_out "out.txt"

let parse ~line =
    	let size = String.length line in 
    	let rec aux i isforward = 
      		if line.[i]=' ' || line.[i]='\t' then 
        		if isforward then aux (i+1) isforward
        		else aux (i-1) isforward
      		else i
    	in
    	let left = aux 0 true in let right =  aux (size-1) false in 
    	(String.sub line left (right-left+1))
;;

(* remove white line and comment line *)
let read_str ~ic = 
    	let rec aux () = 
      		let line = input_line ic in 
         	if String.length line = 0 || line.[0]='\n' || line.[0]='\r' then aux ()
         	else if line.[0]='/' then aux ()
         	else parse line 
    	in
    	aux ()
;;

let setup ~ic = 
    	let trianglenum = 
      		let 
        		line = read_str ~ic 
      		in Scanf.sscanf line "NUMPOLLIES %d" (fun x -> x) 
    	in 
	let vert_list = Array.make trianglenum [|0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0|] |> Array.to_list |> 
			 List.map (fun a -> let line = read_str ~ic in 
            		    Scanf.sscanf line "%f %f %f %f %f %f %f %f" (fun x y z m n k s t -> [|x; y; z; m; n; k; s; t|] ) )  
	in 
	let vertices = Array.concat vert_list in
	Printf.printf "%d\n" (Array.length vertices);
	flush stdout;
    	(vertices , vert_list)
;;

let create_rsm_shader () = 
	let vertex_shader = glCreateShader GL_VERTEX_SHADER in
	let sc = open_in_gen [Open_text] 0o777 "../shader/rsm.vp" in
	let size = in_channel_length sc in
	let vertex_shader_src = String.create size in
	ignore (input sc vertex_shader_src 0 size);
	Printf.fprintf oc "\n\n%s\n\n" vertex_shader_src;
	glShaderSource vertex_shader vertex_shader_src;
	close_in sc;

	let fragment_shader = glCreateShader GL_FRAGMENT_SHADER in
	let sc = open_in_gen [Open_text] 0o777 "../shader/rsm.fp" in
	let size = in_channel_length sc in
	let fragment_shader_src = String.create size in
	ignore (input sc fragment_shader_src 0 size);
	Printf.fprintf oc "\n\n%s\n\n" fragment_shader_src;
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
			   {Glsl_shader.name = "position"; Glsl_shader.value = glGetAttribLocation program "position"};
			   {Glsl_shader.name = "texcoord"; Glsl_shader.value = glGetAttribLocation program "texcoord"} |] 
	in
	let shader_info = {Glsl_shader.vertex_shader; Glsl_shader.fragment_shader; Glsl_shader.geometry_shader = None; 
			   Glsl_shader.program; Glsl_shader.attributes; Glsl_shader.geometry_params = None} in
	Glsl_shader.insert_shader_list "rsm" shader_info
;;


let create_final0_shader () = 
	let vertex_shader = glCreateShader GL_VERTEX_SHADER in
	let sc = open_in_gen [Open_text] 0o777 "../shader/final.vp" in
	let size = in_channel_length sc in
	let vertex_shader_src = String.create size in
	ignore (input sc vertex_shader_src 0 size);
	Printf.fprintf oc "\n\n%s\n\n" vertex_shader_src;
	glShaderSource vertex_shader vertex_shader_src;
	close_in sc;

	let fragment_shader = glCreateShader GL_FRAGMENT_SHADER in
	let sc = open_in_gen [Open_text] 0o777 "../shader/final.fp" in
	let size = in_channel_length sc in
	let fragment_shader_src = String.create size in
	ignore (input sc fragment_shader_src 0 size);
	Printf.fprintf oc "\n\n%s\n\n" fragment_shader_src;
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
			   {Glsl_shader.name = "position"; Glsl_shader.value = glGetAttribLocation program "position"};
			   {Glsl_shader.name = "texcoord"; Glsl_shader.value = glGetAttribLocation program "texcoord"} |] 
	in
	let shader_info = {Glsl_shader.vertex_shader; Glsl_shader.fragment_shader; Glsl_shader.geometry_shader = None; 
			   Glsl_shader.program; Glsl_shader.attributes; Glsl_shader.geometry_params = None} in
	Glsl_shader.insert_shader_list "final0" shader_info
;;



let create_final1_shader () = 
	let vertex_shader = glCreateShader GL_VERTEX_SHADER in
	let sc = open_in_gen [Open_text] 0o777 "../shader/final.vp" in
	let size = in_channel_length sc in
	let vertex_shader_src = String.create size in
	ignore (input sc vertex_shader_src 0 size);
	Printf.fprintf oc "\n\n%s\n\n" vertex_shader_src;
	let sources = [|"#define NO_INDIRECT_LIGHT"; vertex_shader_src|] in
	Glex.glShaderSources vertex_shader 2 (Some sources) None;
	close_in sc;

	let fragment_shader = glCreateShader GL_FRAGMENT_SHADER in
	let sc = open_in_gen [Open_text] 0o777 "../shader/final.fp" in
	let size = in_channel_length sc in
	let fragment_shader_src = String.create size in
	ignore (input sc fragment_shader_src 0 size);
	Printf.fprintf oc "\n\n%s\n\n" fragment_shader_src;
	let sources = [|"#define NO_INDIRECT_LIGHT"; fragment_shader_src|] in
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
			   {Glsl_shader.name = "position"; Glsl_shader.value = glGetAttribLocation program "position"};
			   {Glsl_shader.name = "texcoord"; Glsl_shader.value = glGetAttribLocation program "texcoord"} |] 
	in
	
	let shader_info = {Glsl_shader.vertex_shader; Glsl_shader.fragment_shader; Glsl_shader.geometry_shader = None;
			    Glsl_shader.program; Glsl_shader.attributes; Glsl_shader.geometry_params = None} in
	Glsl_shader.insert_shader_list "final1" shader_info
;;



let create_shader () =
	let rsm_vertex_shader = "../shader/rsm.vp" in
	let rsm_fragment_shader = "../shader/rsm.fp" in
	let shader_info = Glsl_shader.create rsm_vertex_shader rsm_fragment_shader None None None in
	Glsl_shader.insert_shader_list "rsm" shader_info;

	let final0_vertex_shader = "../shader/final.vp" in
	let final0_fragment_shader = "../shader/final.fp" in
	let shader_info = Glsl_shader.create final0_vertex_shader final0_fragment_shader None None None in
	Glsl_shader.insert_shader_list "final0" shader_info;

	let final1_vertex_shader = "../shader/final.vp" in
	let final1_fragment_shader = "../shader/final.fp" in
	let shader_info = Glsl_shader.create final1_vertex_shader final1_fragment_shader None None (Some "#define NO_INDIRECT_LIGHT\n") in
	Glsl_shader.insert_shader_list "final1" shader_info;
;;



let create_model filename =  
	let sc = open_in_gen [Open_text] 0o777 filename in
	let (vertices , vert_list) = setup sc in
	let bbox = Bounding_box.init in
	let bbox = List.fold_left (fun b a -> let c = Bounding_box.add_vertex (a.(0) , a.(1) , a.(2)) b in c) bbox vert_list in
	let decl_array = [|{GL_buffer.field = GL_buffer.VEC3F; GL_buffer.binding_name = "position"};
			   {GL_buffer.field = GL_buffer.VEC3F; GL_buffer.binding_name = "normal"};
			   {GL_buffer.field = GL_buffer.VEC2F; GL_buffer.binding_name = "texcoord"} |] 
	in
	let vb_info = GL_buffer.create_vertex_buffer "test_model_vertex_buffer" decl_array (List.length vert_list) VBO.GL_STATIC_DRAW in
	Printf.printf "%d\n" vb_info.GL_buffer.buffer_size;
	GL_buffer.insert_buffer_list "test_model_vertex_buffer" vb_info;
	GL_buffer.bind_vertex_buffer vb_info;
	GL_buffer.set_vertex_buffer_data 0 vb_info.GL_buffer.buffer_size vertices;
	GL_buffer.unbind_vertex_buffer_with_shader vb_info;
	(bbox, vb_info)	
;;


let create filename = 
	let (bbox,vb_info) = create_model filename in
	create_shader ();
	let (base, params) = match Texture.Resource_Map.find "wall" (!Texture.texture_list) with
				  Texture.Texture_2D (a, b) -> (a, b)
			        | _ -> raise (Failure "wall is a 2-dimension texture.") 
	in
	let wall_texture = base.Texture.tex_id in
	let (base, params) = match Texture.Resource_Map.find "floor" (!Texture.texture_list) with
				  Texture.Texture_2D (a, b) -> (a, b)
			        | _ -> raise (Failure "floor is a 2-dimension texture.") 
	in
	let floor_texture = base.Texture.tex_id in
	{bbox; vb_info; wall_texture; floor_texture}
;;



let draw_model model shader =
	let program = shader.Glsl_shader.program in 	
	let loc = GL.glGetUniformLocation program "diffuse_tex" in
	if loc = -1 then raise (Failure "load diffuse_tex variable failure.");
	GL.glUniform1i loc 0;	
	
	let vertex_buffer = model.vb_info in
	(* bind *)
	GL_buffer.bind_vertex_buffer_with_shader vertex_buffer shader;
	
	(* draw *)
	let (wall_base, wall_params) = match Texture.Resource_Map.find "wall" (!Texture.texture_list) with
				 	 Texture.Texture_2D (a, b) -> (a, b)
			               | _ -> raise (Failure "wall is a 2-dimension texture.") 
	in
	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glBindTexture wall_base.Texture.target wall_base.Texture.tex_id;
	VertArray.glDrawArrays GL.GL_TRIANGLES 72 (vertex_buffer.GL_buffer.length);
		
	let (floor_base, floor_params) = match Texture.Resource_Map.find "floor" (!Texture.texture_list) with
				  Texture.Texture_2D (a, b) -> (a, b)
			        | _ -> raise (Failure "floor is a 2-dimension texture.") 
	in
	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glBindTexture floor_base.Texture.target floor_base.Texture.tex_id;	
	VertArray.glDrawArrays GL.GL_TRIANGLES 36 72;
	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glUnbindTexture floor_base.Texture.target;

	(* unbind *)
	GL_buffer.unbind_vertex_buffer_with_shader vertex_buffer

;;

let draw_floor model shader =
	let program = shader.Glsl_shader.program in 	
	let loc = GL.glGetUniformLocation program "diffuse_tex" in
	if loc = -1 then raise (Failure "load diffuse_tex variable failure.");
	GL.glUniform1i loc 0;	
	
	let vertex_buffer = model.vb_info in
	(* bind *)
	GL_buffer.bind_vertex_buffer_with_shader vertex_buffer shader;
	
	(* draw *)
	let (floor_base, floor_params) = match Texture.Resource_Map.find "floor" (!Texture.texture_list) with
				  Texture.Texture_2D (a, b) -> (a, b)
			        | _ -> raise (Failure "floor is a 2-dimension texture.") 
	in
	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glBindTexture floor_base.Texture.target floor_base.Texture.tex_id;	
	VertArray.glDrawArrays GL.GL_TRIANGLES 0 36;
	GL.glActiveTexture GL.GL_TEXTURE0;
	GL.glUnbindTexture floor_base.Texture.target;

	(* unbind *)
	GL_buffer.unbind_vertex_buffer_with_shader vertex_buffer 
;;


let draw_to_rsm model sun_light rsm = 
	let grid_space_rotation = sun_light.Directional_light.grid_space.Directional_light.rotation in
	let grid_space_translation = sun_light.Directional_light.grid_space.Directional_light.translation in
	let projection = sun_light.Directional_light.grid_space.Directional_light.projection in
	let grid_space = Vector.mult grid_space_translation grid_space_rotation in
(*	
	Vector.print_matrix grid_space_rotation;
	Vector.print_matrix grid_space_translation;
	Vector.print_matrix grid_space;
	Vector.print_matrix projection;	
*)	
(*	Rsm.bind rsm;*)
	FBO.glBindFrameBuffer FBO.GL_FRAMEBUFFER rsm.Rsm.rt.Render_texture.id;
	GL.glViewport 0 0 rsm.Rsm.width rsm.Rsm.height;
	GL.glClearColor 0.0 0.0 0.0 0.0;
	GL.glClear [GL.GL_COLOR_BUFFER_BIT; GL.GL_DEPTH_BUFFER_BIT];
	let rsm_shader = Glsl_shader.Resource_Map.find "rsm" (!Glsl_shader.shader_list) in
	let program = rsm_shader.Glsl_shader.program in
	
	GL.glUseProgram program;

	let loc = GL.glGetUniformLocation program "projection_matrix" in
	if loc = -1 then raise (Failure "load projection_matrix variable failure.");
	GL.glUniformMatrix4f loc false projection;	

	let loc = GL.glGetUniformLocation program "grid_space" in
	if loc = -1 then raise (Failure "load grid_space variable failure.");
	GL.glUniformMatrix4f loc false grid_space;	

	let loc = GL.glGetUniformLocation program "grid_origin_z" in
	if loc = -1 then raise (Failure "load grid_origin_z variable failure.");
	let (x,y,z) = match sun_light.Directional_light.grid_bbox.Bounding_box.min with
				   Vector.Vec3 (a, b, c) -> (a, b, c)
				|  _ -> raise (Failure "unexpected error. should be 3-dimension vector.")	
	in
	GL.glUniform1f loc z;	

	let loc = GL.glGetUniformLocation program "material_color" in
	if loc = -1 then raise (Failure "load material_color variable failure.");
	GL.glUniform4f loc 0.0 0.0 0.0 1.0;	

	draw_model model rsm_shader;
	
	
	let (m_x, m_y, m_z, m_a) = match !material_color with 
				   Vector.Vec4 (a, b, c, d) -> (a, b, c, d)
				|  _ -> raise (Failure "materia color should be 4-dimension vector.")	
	in
	let loc = GL.glGetUniformLocation program "material_color" in
	if loc = -1 then raise (Failure "load material_color variable failure.");
	GL.glUniform4f loc m_x m_y m_z m_a;

	draw_floor model rsm_shader;	
	Rsm.unbind ();
;;


let draw model view_matrix proj_matrix indirect_light_buffer sun_light rsm indirect_light_on = 
	let selected_shader = if indirect_light_on = true then 0 else 1 in
	(* choose one shader between final0 and final1 *)
	let final_shader = if selected_shader = 0 then 
				Glsl_shader.Resource_Map.find "final0" (!Glsl_shader.shader_list)
			   else
				Glsl_shader.Resource_Map.find "final1" (!Glsl_shader.shader_list)
	in
	let program = final_shader.Glsl_shader.program in
	GL.glUseProgram program;
	
	let loc = GL.glGetUniformLocation program "projection_matrix" in
	if loc = -1 then raise (Failure "load projection_matrix variable failure.");
	GL.glUniformMatrix4f loc false proj_matrix;
	
	let loc = GL.glGetUniformLocation program "view_matrix" in
	if loc = -1 then raise (Failure "load view_matrix variable failure.");
	GL.glUniformMatrix4f loc false view_matrix;

	let grid_space_rotation = sun_light.Directional_light.grid_space.Directional_light.rotation in
	let loc = GL.glGetUniformLocation program "light_space_matrix" in
	if loc = -1 then raise (Failure "load light_space_matrix variable failure.");
	GL.glUniformMatrix4f loc false grid_space_rotation;

	let (indirect_light_base, indirect_light_params) = 
		match Texture.Resource_Map.find "indirect_light_buffer" (!Texture.texture_list) with
			   Texture.Texture_2D (base, params) -> (base, params)
			|  _ -> raise (Failure "indirect_light_buffer is 2-dimension texture.")
	
	in	
	if indirect_light_on = true then begin
		let loc = GL.glGetUniformLocation program "indirect_light" in
		if loc = -1 then raise (Failure "load indirect_light variable failure.");
		GL.glUniform1i loc 1;
		
		GL.glActiveTexture GL.GL_TEXTURE1;
		GL.glBindTexture indirect_light_base.Texture.target indirect_light_base.Texture.tex_id;
		 
	end;

	
	let loc = GL.glGetUniformLocation program "grid_origin_z" in
	if loc = -1 then raise (Failure "load grid_origin_z variable failure.");
	let (x, y, z) = match sun_light.Directional_light.grid_bbox.Bounding_box.min with
			  Vector.Vec3 (a, b, c) -> (a, b ,c)
			| _ -> raise (Failure "bounding_box is 3-dimension.")		
	in
	GL.glUniform1f loc z;

	let projection = sun_light.Directional_light.grid_space.Directional_light.projection in
	let rsm_matrix = Vector.mult projection grid_space_rotation in
	let loc = GL.glGetUniformLocation program "rsm_matrix" in
	if loc = -1 then raise (Failure "load rsm_matrix variable failure.");
	GL.glUniformMatrix4f loc false rsm_matrix;

	let loc = GL.glGetUniformLocation program "rsm_depth" in
	if loc = -1 then raise (Failure "load rsm_depth variable failure.");
	GL.glUniform1i loc 2;
	
	let (depth_tex_base, depth_tex_params) = 
		match Texture.Resource_Map.find "rsm_depth_tex" (!Texture.texture_list) with 
		   	  Texture.Texture_2D (base, params) -> (base, params)
			|  _ -> raise (Failure "rsm_depth_tex is 2-dimension texture.")			
	in
	GL.glActiveTexture GL.GL_TEXTURE2;
	GL.glBindTexture depth_tex_base.Texture.target depth_tex_base.Texture.tex_id;
		
	let loc = GL.glGetUniformLocation program "material_color" in
	if loc = -1 then raise (Failure "load indirect_light variable failure.");
	GL.glUniform4f loc 0.0 0.0 0.0 1.0;

	draw_model model final_shader;
	
	let loc = GL.glGetUniformLocation program "material_color" in
	let (m_x, m_y, m_z, m_a) = match !material_color with 
			   Vector.Vec4 (a, b, c, d) -> (a, b, c, d)
			|  _ -> raise (Failure "materia color is a 4 dimension vector.")	
	in
	if loc = -1 then raise (Failure "load indirect_light variable failure.");
	GL.glUniform4f loc m_x m_y m_z m_a;

	draw_floor model final_shader;
	
	FBO.glUnBindFrameBuffer FBO.GL_FRAMEBUFFER;
	
	if indirect_light_on = true then begin
		GL.glActiveTexture GL.GL_TEXTURE1;
		GL.glUnbindTexture indirect_light_base.Texture.target;
	end

;;			
