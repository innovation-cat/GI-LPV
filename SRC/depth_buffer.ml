open FBO

type params = {
		mutable samples : int;
		mutable _format : FBO.render_buffer_object_internal_format;
		mutable stencil : bool;
	      }

type depth_buffer = {
			mutable id    : rbo_id;
			mutable param : params;
		    }

let init_depth_buffer_param = {samples = 1; _format= FBO.GL_DEPTH_COMPONENT; stencil=false}

let create params width height = 
	let id = FBO.glGenRenderBuffers 1 in
	glBindRenderBuffer FBO.GL_RENDERBUFFER id.(0);
	
	if params.samples > 1 then 
		FBO.glRenderbufferStorageMultisample FBO.GL_RENDERBUFFER params.samples params._format width height
	else 
		FBO.glRenderbufferStorage FBO.GL_RENDERBUFFER params._format width height;
	{id=id.(0); param = params}	
