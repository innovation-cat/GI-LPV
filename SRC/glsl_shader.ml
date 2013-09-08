open GL

type attribute = {mutable name : string; mutable value : int}

type shader = { mutable vertex_shader   : shader_object;
	        mutable fragment_shader : shader_object;
	        mutable geometry_shader : shader_object;
	        mutable program         : shader_program;
		mutable attributes      : attribute array
              }

module Resource_Map = Map.Make (struct type t = string let compare = String.compare end)

let shader_list = ref (Resource_Map.empty)

let insert_shader_list (name:string) (shader_info:shader) = shader_list := Resource_Map.add name shader_info (!shader_list)


