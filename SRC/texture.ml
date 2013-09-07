open GL

exception FBO_Not_Support;;

module Resource_Map = Map.Make (struct type t = string let compare = String.compare end);;


type texture_base = {
			mutable tex_id 	      : texture_id; (* type texture_id = private int *)
			mutable target 	      : BindTex.texture_binding;
			mutable name          : string;
	       	    }

module Texture_Params_2D = struct
	type texture_params_2d = {
					mutable target               : TexParam.tex_param_target;
					mutable wrap_s               : wrap_param;
					mutable wrap_t 		     : wrap_param;
					mutable mag_filter 	     : GL.Mag.mag_filter;
					mutable min_filter 	     : GL.Min.min_filter; 
					mutable source_format        : pixel_data_format;
					mutable internal_format      : InternalFormat.internal_format;
					mutable n_type               : pixel_data_type;
					mutable degree_of_anisotropy : int
			 	}

	let init_param_2d = {
				target = TexParam.GL_TEXTURE_2D;
				wrap_s = GL_CLAMP_TO_EDGE;
				wrap_t = GL_CLAMP_TO_EDGE;
				mag_filter = GL.Mag.GL_LINEAR;
				min_filter = GL.Min.GL_LINEAR;
				source_format = GL_RGB;
				internal_format = InternalFormat.GL_RGB;
				n_type = GL_UNSIGNED_BYTE;
				degree_of_anisotropy = 0
			    }
end

module Texture_Params_3D = struct
	type texture_params_3d = {			
					mutable target               : TexParam.tex_param_target;
					mutable wrap_r               : wrap_param;
					mutable wrap_s               : wrap_param;
					mutable wrap_t 		     : wrap_param;
					mutable mag_filter 	     : GL.Mag.mag_filter;
					mutable min_filter 	     : GL.Min.min_filter; 
					mutable source_format        : pixel_data_format;
					mutable internal_format      : InternalFormat.internal_format;
					mutable n_type               : pixel_data_type;
			 	}


	let init_param_3d = {
				target = TexParam.GL_TEXTURE_3D;
				wrap_s = GL_CLAMP;
				wrap_t = GL_CLAMP;
				wrap_r = GL_CLAMP;
				mag_filter = GL.Mag.GL_LINEAR;
				min_filter = GL.Min.GL_LINEAR;
				source_format = GL_RGB;
				internal_format = InternalFormat.GL_RGB;
				n_type = GL_UNSIGNED_BYTE;
			    }
end

type texture = Texture_2D of texture_base*Texture_Params_2D.texture_params_2d | Texture_3D of texture_base*Texture_Params_3D.texture_params_3d

let texture_list = ref Resource_Map.empty

(* create new 2d texture, and insert into texture_list *)
let create_texture_2d ~base ~params ~width ~height ~pixels =
	 (* check fbo support *)
	glBindTexture base.target base.tex_id;
(*	glPixelStorei GL_UNPACK_ALIGNMENT 1;*)
	glTexParameter params.Texture_Params_2D.target (TexParam.GL_TEXTURE_WRAP_S params.Texture_Params_2D.wrap_s);
	glTexParameter params.Texture_Params_2D.target (TexParam.GL_TEXTURE_WRAP_T params.Texture_Params_2D.wrap_t);
	glTexParameter params.Texture_Params_2D.target (TexParam.GL_TEXTURE_MAG_FILTER params.Texture_Params_2D.mag_filter);
	glTexParameter params.Texture_Params_2D.target (TexParam.GL_TEXTURE_MIN_FILTER params.Texture_Params_2D.min_filter);
	
	glTexImage2D TexTarget.GL_TEXTURE_2D 0 params.Texture_Params_2D.internal_format width height params.Texture_Params_2D.source_format params.Texture_Params_2D.n_type pixels;
	(*glGenerateMipmapEXT base.target;*)
	glUnbindTexture base.target;
	
	texture_list := Resource_Map.add base.name (base,params) !texture_list
;;
	
