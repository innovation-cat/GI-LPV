// pixel shader

uniform sampler2D diffuse_tex;
#ifndef NO_INDIRECT_LIGHT
uniform sampler2D indirect_light;
#endif

// reuse rsm depth texture as shadow map
uniform sampler2D rsm_depth;

uniform vec4 material_color;

varying vec2 tc;
varying float direct;
#ifndef NO_INDIRECT_LIGHT
varying vec3 indirect_light_tc;
#endif
varying vec3 rsm_pos;

void main()
{
	float depth = texture2D(rsm_depth, rsm_pos.xy).r;
	
	// rsm_pos.z < depth -> 0
	float shadow = step(depth, rsm_pos.z + 0.5);

	const float ambient = 0.05;
	const float one_minus_ambient = 1.0 - ambient;

	// light = direct light * shadow + ambient
	vec3 light = vec3(direct) * shadow + vec3(ambient);
#ifndef NO_INDIRECT_LIGHT
	// sample indirect light value and add
	vec3 indirect = texture2DProj(indirect_light, indirect_light_tc.xyzz).rgb;
	light += indirect * one_minus_ambient;
#endif

	// get surface color
	vec3 color = texture2D(diffuse_tex, tc).rgb * material_color.a + material_color.rgb;

	vec3 c = color * light;

	gl_FragColor.rgb = c; 
}
