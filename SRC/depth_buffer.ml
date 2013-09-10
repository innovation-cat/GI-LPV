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
