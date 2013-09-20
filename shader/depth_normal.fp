// pixel shader

varying vec3 view_space_normal;
varying float view_space_depth; 

void main()
{
	// store view space normal and depth in an RGBA8 texture
	vec2 packed_depth;
	packed_depth.r = floor(view_space_depth * 255.0) / 255.0;
	packed_depth.g = fract(view_space_depth * 255.0);

	vec2 normal = vec2(normalize(view_space_normal) );
	normal = normal * 0.5 + 0.5;

	gl_FragColor = vec4(packed_depth, normal);
}
