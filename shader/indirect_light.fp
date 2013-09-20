// pixel shader

// light intensity stored as spherical harmonics
uniform sampler3D incoming_red;
uniform sampler3D incoming_green; 
uniform sampler3D incoming_blue;

// offset to sample neighbouring cells
#ifdef DAMPEN
uniform vec3 offset_along_normal;
#endif
varying vec3 light_space_normal;
varying vec3 grid_coords;

vec3 calc_indirect_lighting(in vec3 grid_coords, in vec4 transfer_function)
{
	vec3 indirect;
	// find dampening factor based on the directional derivative of the intensity distribution
	// to reduce light bleeding through thin walls
	vec4 red = texture3D(incoming_red, grid_coords);
	vec4 green = texture3D(incoming_green, grid_coords);
	vec4 blue = texture3D(incoming_blue, grid_coords);

#ifdef DAMPEN
	vec3 offset = light_space_normal * offset_along_normal;
	vec3 sample_location1 = grid_coords + offset; // in front of the surface
	vec3 sample_location2 = grid_coords - offset; // behind the surface

	vec4 neighbour1_red = texture3D(incoming_red, sample_location1);
	vec4 neighbour1_green = texture3D(incoming_green, sample_location1);
	vec4 neighbour1_blue = texture3D(incoming_blue, sample_location1);
	vec4 neighbour2_red = texture3D(incoming_red, sample_location2);
	vec4 neighbour2_green = texture3D(incoming_green, sample_location2);
	vec4 neighbour2_blue = texture3D(incoming_blue, sample_location2);

	vec4 diff_red = (neighbour1_red - neighbour2_red);
	vec4 diff_green = (neighbour1_green - neighbour2_green);
	vec4 diff_blue = (neighbour1_blue - neighbour2_blue);

	// this is the method I came up with, doesn't work well in some situations

	// this is supposed to indicate if light comes from "behind" the wall
	vec3 sh_diff = vec3(dot(diff_red, red), dot(diff_green, green), dot(diff_blue, blue) );

	const vec3 dampening_max = vec3(1.0);

	vec3 step = vec3(1.0) - step(vec3(0.0), sh_diff);

	// how much light comes from behind the wall?
	vec3 dampening_mag = vec3(dot(diff_red, diff_red), dot(diff_green, diff_green), dot(diff_blue, diff_blue) );

	// this is to reduce indirect light thus removing the light from "behind" the wall 
	vec3 dampening = vec3(1.0) - step * sqrt(dampening_mag) / dampening_max;
	
    dampening = max(dampening, 0.2);

	// calculate amount of indirect light reaching this surface point 
	indirect.r = dot(red * dampening.r, transfer_function);
	indirect.g = dot(green * dampening.g, transfer_function);
	indirect.b = dot(blue * dampening.b, transfer_function);
#else
	indirect.r = dot(red, transfer_function);
	indirect.g = dot(green, transfer_function);
	indirect.b = dot(blue, transfer_function);
#endif
    indirect = max(indirect, 0.0);

	return indirect;
}

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
	vec4 transfer_function = create_vpl(-light_space_normal);
	
	vec3 indirect = calc_indirect_lighting(grid_coords, transfer_function);

	gl_FragColor.rgb = indirect; 
}
