
open GL

external glShaderSources : GL.shader_object -> int -> string array option -> int array option -> unit = "ml_glshadersources"
