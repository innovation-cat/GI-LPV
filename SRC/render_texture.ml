(* render_texture used to attach to color frame buffer *)
open GL
open Texture
open FBO
open Glex
open Depth_buffer

(* type render_textures = Texture.texture_id list *)

type render_textures = {
				mutable tex_list : GL.texture_id array;
				mutable depth_tex_list : GL.texture_id array
		       }

let draw_buffer_samples = [| Glex.GL_COLOR_ATTACHMENT0 
   ;   Glex.GL_COLOR_ATTACHMENT1
   ;   Glex.GL_COLOR_ATTACHMENT2
   ;   Glex.GL_COLOR_ATTACHMENT3
   ;   Glex.GL_COLOR_ATTACHMENT4
   ;   Glex.GL_COLOR_ATTACHMENT5
   ;   Glex.GL_COLOR_ATTACHMENT6
   ;   Glex.GL_COLOR_ATTACHMENT7
   ;   Glex.GL_COLOR_ATTACHMENT8 
   ;   Glex.GL_COLOR_ATTACHMENT9
   ;   Glex.GL_COLOR_ATTACHMENT10
   ;   Glex.GL_COLOR_ATTACHMENT11
   ;   Glex.GL_COLOR_ATTACHMENT12
   ;   Glex.GL_COLOR_ATTACHMENT13 
   ;   Glex.GL_COLOR_ATTACHMENT14
   ;   Glex.GL_COLOR_ATTACHMENT15 |] 

let draw_buffer_to_frame_buffer_attachment = [| FBO.GL_COLOR_ATTACHMENT0 
   ;   FBO.GL_COLOR_ATTACHMENT1
   ;   FBO.GL_COLOR_ATTACHMENT2
   ;   FBO.GL_COLOR_ATTACHMENT3
   ;   FBO.GL_COLOR_ATTACHMENT4
   ;   FBO.GL_COLOR_ATTACHMENT5
   ;   FBO.GL_COLOR_ATTACHMENT6
   ;   FBO.GL_COLOR_ATTACHMENT7
   ;   FBO.GL_COLOR_ATTACHMENT8 
   ;   FBO.GL_COLOR_ATTACHMENT9
   ;   FBO.GL_COLOR_ATTACHMENT10
   ;   FBO.GL_COLOR_ATTACHMENT11
   ;   FBO.GL_COLOR_ATTACHMENT12
   ;   FBO.GL_COLOR_ATTACHMENT13 
   ;   FBO.GL_COLOR_ATTACHMENT14
   ;   FBO.GL_COLOR_ATTACHMENT15 |] 
let create_draw_buffers n = Array.sub draw_buffer_samples 0 n

(* render_textures -> Depth_buffer.depth_buffer -> unit *)
let create_render_texture textures dp = 
	let fbo = FBO.glGenFrameBuffers 1 in
	FBO.glBindFrameBuffer FBO.GL_FRAMEBUFFER fbo.(0);
	let size = Array.length textures.tex_list in
	let draw_buffers = create_draw_buffers size in
	if size = 0 then GL.glDrawBuffer GL.DrawBuffer.GL_NONE
	else Glex.glDrawBuffers size draw_buffers;
	Array.iteri (fun i tex -> glFrameBufferTexture FBO.GL_FRAMEBUFFER draw_buffer_to_frame_buffer_attachment.(i) tex 0) textures.tex_list; 
	Array.iteri (fun i tex -> glFrameBufferTexture2D FBO.GL_FRAMEBUFFER FBO.GL_DEPTH_ATTACHMENT FBO.GL_TEXTURE_2D tex 0) textures.depth_tex_list; 
	
	begin
		match dp with
		   None -> ()
		|  Some p -> if p.Depth_buffer.param.Depth_buffer.stencil=true then 
				glFramebufferRenderbuffer FBO.GL_FRAMEBUFFER FBO.GL_DEPTH_STENCIL_ATTACHMENT FBO.GL_RENDERBUFFER p.Depth_buffer.id
			     else
				glFramebufferRenderbuffer FBO.GL_FRAMEBUFFER FBO.GL_DEPTH_ATTACHMENT FBO.GL_RENDERBUFFER p.Depth_buffer.id
	end;
	
	glUnBindFrameBuffer FBO.GL_FRAMEBUFFER

	
