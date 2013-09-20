// geometry shader

#extension EXT_gpu_shader4 : require

varying in vec4 blocker[1];
varying out vec4 blocking_potential;

void main() 
{	
	blocking_potential = blocker[0];

	gl_Position = vec4(gl_PositionIn[0].xy * 2.0 - 1.0, 0.0, 1.0);
	gl_Layer = int(gl_PositionIn[0].z);
	gl_PointSize = 1;
	EmitVertex();
	EndPrimitive();
}
