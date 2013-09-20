#define NO_BLOCKING

// geometry shader

#extension GL_EXT_gpu_shader4 : enable

varying in vec3 grid_tex_coord[3];
varying out vec3 tex_coord;

void main() 
{
	// output primitive to process a layer of the 3D texture
	for (int i = 0;i < 3;i++) 
	{	
		tex_coord = grid_tex_coord[i]; 
		gl_Position = vec4(2.0 * gl_PositionIn[i].xy - 1.0, 0.0, 1.0);	
		gl_Layer = int(gl_PositionIn[i].z); // layer of the 3D texture
		EmitVertex();
	}
	
	EndPrimitive();
}
