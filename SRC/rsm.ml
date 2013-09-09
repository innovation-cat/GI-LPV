(* reflective shadow map *)

open GL
open FBO
open Texture

type t = {
		mutable width       : int;
		mutable height      : int;
		mutable normal_tex  : GL.texture_id; 
		mutable color_tex   : GL.texture_id;
		mutable depth_tex   : GL.texture_id;
		mutable depthbuffer : FBO.rbo_id;
		mutable rt          : FBO.fbo_id;		
	 }

let create_rsm width height = 
	let base = {Texture.tex_id = glGenTexture (); Texture.target = BindTex.GL_TEXTURE_2D ; Texture.name = "rsm_normal_tex"} in
	let params = {Texture.init_param_2d with min_filter = GL.Min.GL_NEAREST; 
					 mag_filter = GL.Mag.GL_NEAREST;
		    		         source_format = if bpp=4 then GL_RGBA else GL_RGB;
			        	 internal_format = if bpp=4 then InternalFormat.GL_RGBA else InternalFormat.GL_RGB;
				         wrap_s = GL_REPEAT;
				         wrap_t = GL_REPEAT;
					 degree_of_anisotropy = 1}
	in 
	Texture.create_texture_2d ~base ~params ~width ~height ~pixels;

