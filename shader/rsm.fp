// pixel shader

varying vec2 grid_space_normal;
varying vec2 tc;
varying float depth;

uniform sampler2D diffuse_tex; 
uniform vec4 material_color;

void main()
{
	// write color, normal in grid space and depth in grid space into RSM
	vec3 diffuse = texture2D(diffuse_tex, tc).rgb * material_color.a + material_color.rgb;
	gl_FragData[0].rg = grid_space_normal;
	gl_FragData[1].rgb = diffuse;
	gl_FragData[2].r = depth;
}
