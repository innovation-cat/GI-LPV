// geometry shader

#extension EXT_gpu_shader4 : require

varying in vec2 texcoord[1];
varying in vec3 norm[1];

varying out vec2 tc;
varying out vec3 normal;

void main() 
{
	tc = texcoord[0];
	normal = norm[0];
	gl_Position = vec4(gl_PositionIn[0].xy, 0.0, 1.0);
	// layer within 3D texture
	gl_Layer = int(gl_PositionIn[0].z);
	gl_PointSize = 1;
	EmitVertex();
	EndPrimitive();
}
