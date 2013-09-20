// pixel shader

varying vec2 tc;
varying vec3 normal;

uniform sampler2D color_tex;
uniform float point_weight;
uniform vec3 light_dir;

vec3 rotate_zh_cone_coeff(in vec3 dir) 
{ 
	const float zh_second_band = 1.0233266;

	vec2 theta12_cs = normalize(dir.xz);
	vec2 phi12_cs = vec2(sqrt(1.0 - dir.y * dir.y), dir.y); 	

	vec3 rotated_coeffs;

	rotated_coeffs.x = zh_second_band * phi12_cs.x * theta12_cs.y; 
	rotated_coeffs.y = zh_second_band * phi12_cs.y; 
	rotated_coeffs.z = -zh_second_band * phi12_cs.x * theta12_cs.x;
	
	return rotated_coeffs;
}

vec4 create_vpl(in vec3 normal)
{
	// clamped cosine function (see jose.pdf page 5)
	const float zh_first_band = 0.88622689;
	const float zh_second_band = 1.0233266;

	return (abs(normal.y) < 0.99) ? vec4(zh_first_band, rotate_zh_cone_coeff(normal) ) : vec4(zh_first_band, 0.0, zh_second_band * sign(normal.y), 0.0);
}

void main()
{	
	float intensity = max(0.0, dot(-light_dir, normal) );

	// create virtual point light
	vec4 sh = create_vpl(normal);
	vec3 color = texture2D(color_tex, tc).rgb * intensity * point_weight;
		
	gl_FragData[0] = color.r * sh;
	gl_FragData[1] = color.g * sh;
	gl_FragData[2] = color.b * sh;			
}
