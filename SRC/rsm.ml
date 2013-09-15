(* reflective shadow map *)

open GL
open FBO
open Texture
open Glex
open Depth_buffer
open Render_texture

type t = {
		mutable width       : int;
		mutable height      : int;
		mutable normal_tex  : GL.texture_id; 
		mutable color_tex   : GL.texture_id;
		mutable depth_tex   : GL.texture_id;
		mutable depthbuffer : FBO.rbo_id;
		mutable rt          : FBO.fbo_id;		
	 }

let create width height = 
	let normal_base = {Texture.tex_id = glGenTexture (); Texture.target = BindTex.GL_TEXTURE_2D ; Texture.name = "rsm_normal_tex"} in
	let normal_params = {Texture.init_param_2d with Texture_Params_2D.min_filter = GL.Min.GL_NEAREST; 
					 Texture_Params_2D.mag_filter = GL.Mag.GL_NEAREST;
		    		         Texture_Params_2D.n_type = GL.GL_FLOAT;
			        	 Texture_Params_2D.internal_format = Glex.GL_RG16F
		    	    }
	in 
	Texture.create_texture_2d normal_base normal_params width height None;
	
	let color_base = {Texture.tex_id = glGenTexture (); Texture.target = BindTex.GL_TEXTURE_2D ; Texture.name = "rsm_color_tex"} in
	let color_params = {normal_params with Texture_Params_2D.n_type = GL.GL_UNSIGNED_BYTE; Texture_Params_2D.internal_format = Glex.GL_RGB8 } in 
	Texture.create_texture_2d color_base color_params width height None;

	let depth_base = {Texture.tex_id = glGenTexture (); Texture.target = BindTex.GL_TEXTURE_2D ; Texture.name = "rsm_depth_tex"} in
	let depth_params = {color_params with Texture_Params_2D.n_type = GL.GL_FLOAT; Texture_Params_2D.internal_format = Glex.GL_R16F } in 
	Texture.create_texture_2d depth_base depth_params width height None;
	
	(* create render buffer object *)
	let depth_buffer_params = Depth_buffer.init_depth_buffer_param in
	let depthbuffer = Depth_buffer.create depth_buffer_params width height in  
(*	let depth_buffer_params = Depth_buffer.init_depth_buffer_param in
	let depth_buffer_id = FBO.glGenRenderBuffers 1 in
	glBindRenderBuffer FBO.GL_RENDERBUFFER depth_buffer_id.(0);
	
	if depth_buffer_params.Depth_buffer.samples > 1 then 
		FBO.glRenderbufferStorageMultisample FBO.GL_RENDERBUFFER depth_buffer_params.Depth_buffer.samples depth_buffer_params.Depth_buffer._format width height
	else 
		FBO.glRenderbufferStorage FBO.GL_RENDERBUFFER depth_buffer_params.Depth_buffer._format width height;
	let depthbuffer = {Depth_buffer.id = depth_buffer_id.(0); Depth_buffer.param = depth_buffer_params} in
*)
	(* create framebuffer, and make depth_buffer attach to depth frame buffer, while normal_tex, color_tex and depth_tex *)
	(* are attach to color frame buffer                                                                                  *)
	let textures = 	{  
				 Render_texture.tex_list = [|normal_base.Texture.tex_id; color_base.Texture.tex_id; depth_base.Texture.tex_id|];
				 Render_texture.depth_tex_list = [||];
			}
	in
	let rt = Render_texture.create textures (Some depthbuffer) in
	{
	  width; height; 
	  normal_tex = normal_base.Texture.tex_id; 
          color_tex = color_base.Texture.tex_id;
	  depth_tex = depth_base.Texture.tex_id;
	  depthbuffer = depthbuffer.Depth_buffer.id; rt;
	}
	
let bind data = FBO.glBindFrameBuffer FBO.GL_FRAMEBUFFER data.rt

let unbind () = FBO.glUnBindFrameBuffer FBO.GL_FRAMEBUFFER
