#define NO_BLOCKING

// pixel shader

// light intensity stored as spherical harmonics
uniform sampler3D coeffs_red;
uniform sampler3D coeffs_green; 
uniform sampler3D coeffs_blue; 
// geometry volume grid
#ifndef NO_BLOCKING
uniform sampler3D geometry_volume;
#endif

uniform vec3 inv_grid_size;
#ifndef NO_BLOCKING
uniform vec3 proj_grid_to_gv[2];
uniform float half_texel_size;
#endif

varying vec3 tex_coord;

// calculate incident flux for a face of the destination cell
void create_vpl(in vec4 red, 
			    in vec4 green, 
			    in vec4 blue, 
			    in vec4 central_direction_sh_basis,
			    in vec4 vpl,
			    in float solid_angle,
			    out vec4 contribution_red, 
			    out vec4 contribution_green, 
			    out vec4 contribution_blue)
{
	// calculate avarage light intensity
	vec3 intensity;	
	intensity.r = max(0.0, dot(central_direction_sh_basis, red) );
	intensity.g = max(0.0, dot(central_direction_sh_basis, green) );
	intensity.b = max(0.0, dot(central_direction_sh_basis, blue) );
				
	// multiply by solid angle to calculate the flux
	vec3 flux = intensity * solid_angle;

	// create virtual point light pointing towards the face
	contribution_red = vpl * flux.r;
	contribution_green = vpl * flux.g;
	contribution_blue = vpl * flux.b;
}

// read sh vectors storing the light intensity
void get_vpls(vec3 source_cell, out vec4 red, out vec4 green, out vec4 blue)
{
	red = texture3D(coeffs_red, source_cell);
	green = texture3D(coeffs_green, source_cell);
	blue = texture3D(coeffs_blue, source_cell);
}

#ifndef NO_BLOCKING
// read sh vector storing the blocking potential
vec4 sample_blocking_potential(vec3 sample_pos)
{
	vec3 gv_sample_pos = sample_pos * proj_grid_to_gv[0].xyz + proj_grid_to_gv[1].xyz;
	vec4 blocking_potential = texture3D(geometry_volume, gv_sample_pos);
	return blocking_potential;
}

// apply blocking potential to reduce amout of light being propagated through blockers
void apply_blocking_potential(in vec4 light, in vec3 sample_pos, inout vec4 vpl_r, inout vec4 vpl_g, inout vec4 vpl_b)
{
	const float blocking_max = 1.83;

	vec4 blocking_potential = sample_blocking_potential(sample_pos);

	// value depends on direction of light and blocking potential and magnitude of blocking potential  
	float b = max(dot(blocking_potential, light), 0.0);
	
	float c = 1.0 - b / blocking_max;
	vpl_r = vpl_r * c;
	vpl_g = vpl_g * c;
	vpl_b = vpl_b * c;
}
#endif

// propagate light from neighouring cell to the 5 faces of this cell
void propagate(in vec4 red, // vpl of neighbour
			   in vec4 green, // vpl of neighbour 
			   in vec4 blue, // vpl of neighbour

			   in vec4 central_direction_sh_basis1,
			   in vec4 vpl1,
			   in float solid_angle1,

			   in vec4 central_direction_sh_basis2,
			   in vec4 vpl2,
			   in float solid_angle2,

			   in vec4 central_direction_sh_basis3,
			   in vec4 vpl3,
			   in float solid_angle3,

			   in vec4 central_direction_sh_basis4,
			   in vec4 vpl4,
			   in float solid_angle4,

			   in vec4 central_direction_sh_basis5,
			   in vec4 vpl5,
			   in float solid_angle5,

			   out vec4 vpl_red, 
			   out vec4 vpl_green, 
			   out vec4 vpl_blue)
{
	vpl_red = vec4(0.0);
	vpl_green = vec4(0.0);
	vpl_blue = vec4(0.0);

	vec4 contribution_red;
	vec4 contribution_green;
	vec4 contribution_blue;
	
	// create virtual point light pointing towards face 1
	create_vpl(red, green, blue, central_direction_sh_basis1, vpl1, solid_angle1, contribution_red, contribution_green, contribution_blue);
	vpl_red += contribution_red;
	vpl_green += contribution_green;
	vpl_blue += contribution_blue;

	// create virtual point light pointing towards face 2
	create_vpl(red, green, blue, central_direction_sh_basis2, vpl2, solid_angle2, contribution_red, contribution_green, contribution_blue);	
	vpl_red += contribution_red;
	vpl_green += contribution_green;
	vpl_blue += contribution_blue;

	// create virtual point light pointing towards face 3	
	create_vpl(red, green, blue, central_direction_sh_basis3, vpl3, solid_angle3, contribution_red, contribution_green, contribution_blue);	
	vpl_red += contribution_red;
	vpl_green += contribution_green;
	vpl_blue += contribution_blue;
	
	// create virtual point light pointing towards face 4
	create_vpl(red, green, blue, central_direction_sh_basis4, vpl4, solid_angle4, contribution_red, contribution_green, contribution_blue);	
	vpl_red += contribution_red;
	vpl_green += contribution_green;
	vpl_blue += contribution_blue;
	
	// create virtual point light pointing towards face 5
	create_vpl(red, green, blue, central_direction_sh_basis5, vpl5, solid_angle5, contribution_red, contribution_green, contribution_blue);	
	vpl_red += contribution_red;
	vpl_green += contribution_green;
	vpl_blue += contribution_blue;
}

void main()
{
	vec4 total_vpl_r = vec4(0.0);
	vec4 vpl_r;

	vec4 total_vpl_g = vec4(0.0);
	vec4 vpl_g;

	vec4 total_vpl_b = vec4(0.0);
	vec4 vpl_b;


	vec3 sample_pos_neg = tex_coord - inv_grid_size;
	vec3 sample_pos_pos = tex_coord + inv_grid_size;

	vec3 grid_sample_pos;
	vec4 red_in, green_in, blue_in;
	float s;

	// get light intensity sh vectors from the cell left to this cell
	grid_sample_pos = vec3(sample_pos_neg.x, tex_coord.yz);
	get_vpls(grid_sample_pos, red_in, green_in, blue_in);
#ifndef NO_BLOCKING
	grid_sample_pos.x += half_texel_size; // sample pos of blocking potential is between both cells
	apply_blocking_potential(vec4(0.88622689, 0.0, 0.0, -1.0233266), grid_sample_pos, red_in, green_in, blue_in);
#endif

	const vec4 sh_basic_central_dir1 = vec4(0.28209481, 0.0, 0.25687355, -0.41563013);
	const vec4 vpl1 = vec4(0.88622689, 0.0, 1.0233266, 0.0);
	const float solid_angle1 = 0.42343098;

	const vec4 sh_basic_central_dir2 = vec4(0.28209481, 0.0, -0.25687355, -0.41563013);
	const vec4 vpl2 = vec4(0.88622689, 0.0, -1.0233266, 0.0);
	const float solid_angle2 = solid_angle1;

	const vec4 sh_basic_central_dir3 = vec4(0.28209481, 0.0, 0.0, -0.48860252);
	const vec4 vpl3 = vec4(0.88622689, 0.0, 0.0, -1.0233266);
	const float solid_angle3 = 0.40067077;

	const vec4 sh_basic_central_dir4 = vec4(0.28209481, 0.25687355, 0.0, -0.41563013);
	const vec4 vpl4 = vec4(0.88622689, 1.0233266, 0.0, 0.0);
	const float solid_angle4 = solid_angle1;

	const vec4 sh_basic_central_dir5 = vec4(0.28209481, -0.25687355, 0.0, -0.41563013);
	const vec4 vpl5 = vec4(0.88622689, -1.0233266, 0.0, 0.0);
	const float	solid_angle5 = solid_angle1;

	// propagate light from adjacent cell to the five faces of this cell
	propagate(red_in, 
			  green_in, 
			  blue_in,

			  sh_basic_central_dir1,
			  vpl1,
			  solid_angle1,
			  sh_basic_central_dir2,
			  vpl2,
			  solid_angle2,
			  sh_basic_central_dir3,
			  vpl3,
			  solid_angle3,
			  sh_basic_central_dir4,
			  vpl4,
			  solid_angle4,
			  sh_basic_central_dir5,
			  vpl5,
			  solid_angle5,

			  vpl_r, 
			  vpl_g, 
			  vpl_b);


	s = step(0.0, sample_pos_neg.x);
	total_vpl_r = vpl_r * s;
	total_vpl_g = vpl_g * s;
	total_vpl_b = vpl_b * s;
	
	// get light intensity sh vectors from the cell right to this cell
	grid_sample_pos = vec3(sample_pos_pos.x, tex_coord.yz);
	get_vpls(grid_sample_pos, red_in, green_in, blue_in);
#ifndef NO_BLOCKING
	grid_sample_pos.x -= half_texel_size; //sample pos of blocking potential is between both cells
	apply_blocking_potential(vec4(0.88622689, 0.0, 0.0, 1.0233266), grid_sample_pos, red_in, green_in, blue_in);
#endif

	const vec4 sh_basic_central_dir6 = vec4(0.28209481, 0.0, 0.25687355, 0.41563013);
	const vec4 vpl6 = vpl1;
	const float	solid_angle6 = solid_angle1;

	const vec4 sh_basic_central_dir7 = vec4(0.28209481, 0.0, -0.25687355, 0.41563013);
	const vec4 vpl7 = vpl2;
	const float solid_angle7 = solid_angle1;

	const vec4 sh_basic_central_dir8 = vec4(0.28209481, 0.0, 0.0, 0.48860252);
	const vec4 vpl8 = vec4(0.88622689, 0.0, 0.0, 1.0233266);
	const float solid_angle8 = solid_angle3;

	const vec4 sh_basic_central_dir9 = vec4(0.28209481, 0.25687355, 0.0, 0.41563013);
	const vec4 vpl9 = vpl4;
	const float solid_angle9 = solid_angle1;

	const vec4 sh_basic_central_dir10 = vec4(0.28209481, -0.25687355, 0.0, 0.41563013);
	const vec4 vpl10 = vpl5;
	const float	solid_angle10 =	solid_angle1;

	// propagate light from adjacent cell to the five faces of this cell
	propagate(red_in,
			  green_in, 
			  blue_in, 

			  sh_basic_central_dir6,
			  vpl6,
			  solid_angle6,
			  sh_basic_central_dir7,
			  vpl7,
			  solid_angle7,
			  sh_basic_central_dir8,
			  vpl8,
			  solid_angle8,
			  sh_basic_central_dir9,
			  vpl9,
			  solid_angle9,
			  sh_basic_central_dir10,
			  vpl10,
			  solid_angle10,
			   
			  vpl_r, 
			  vpl_g, 
			  vpl_b);

	s = step(sample_pos_pos.x, 1.0);
	total_vpl_r += vpl_r * s;
	total_vpl_g += vpl_g * s;
	total_vpl_b += vpl_b * s;

	// get light intensity sh vectors from the cell above this cell
	grid_sample_pos = vec3(tex_coord.x, sample_pos_pos.y, tex_coord.z);
	get_vpls(grid_sample_pos, red_in, green_in, blue_in);
#ifndef NO_BLOCKING
	grid_sample_pos.y -= half_texel_size; //sample pos of blocking potential is between both cells
	apply_blocking_potential(vec4(0.88622689, 0.0, -1.0233266, 0.0), grid_sample_pos, red_in, green_in, blue_in);
#endif

	const vec4 sh_basic_central_dir16 = vec4(0.28209481, 0.0, -0.48860252, 0.0);
	const vec4 vpl16 = vpl2;
	const float solid_angle16 =	solid_angle3;

	const vec4 sh_basic_central_dir17 = vec4(0.28209481, 0.0, -0.41563013, -0.25687355);
	const vec4 vpl17 = vpl3;
	const float	solid_angle17 = solid_angle1;

	const vec4 sh_basic_central_dir18 = vec4(0.28209481, 0.0, -0.41563013, 0.25687355);
	const vec4 vpl18 = vpl8;
	const float	solid_angle18 = solid_angle1;

	const vec4 sh_basic_central_dir19 = vec4(0.28209481, 0.25687355, -0.41563013, 0.0);
	const vec4 vpl19 = vpl4;
	const float	solid_angle19 =	solid_angle1;

	const vec4 sh_basic_central_dir20 = vec4(0.28209481, -0.25687355, -0.41563013, 0.0);
	const vec4 vpl20 = vpl5;
	const float	solid_angle20 = solid_angle1;

	// propagate light from adjacent cell to the five faces of this cell
	propagate(red_in, 
			  green_in, 
			  blue_in, 

			  sh_basic_central_dir16,
			  vpl16,
			  solid_angle16,
			  sh_basic_central_dir17,
			  vpl17,
			  solid_angle17,
			  sh_basic_central_dir18,
			  vpl18,
			  solid_angle18,
			  sh_basic_central_dir19,
			  vpl19,
			  solid_angle19,
			  sh_basic_central_dir20,
			  vpl20,
			  solid_angle20,
			  
			  vpl_r, 
			  vpl_g, 
			  vpl_b);

	s = step(sample_pos_pos.y, 1.0);
	total_vpl_r += vpl_r * s;
	total_vpl_g += vpl_g * s;
	total_vpl_b += vpl_b * s;

	// get light intensity sh vectors from the cell below this cell
	grid_sample_pos = vec3(tex_coord.x, sample_pos_neg.y, tex_coord.z);
	get_vpls(grid_sample_pos, red_in, green_in, blue_in);
#ifndef NO_BLOCKING
	grid_sample_pos.y += half_texel_size; //sample pos of blocking potential is between both cells
	apply_blocking_potential(vec4(0.88622689, 0.0, 1.0233266, 0.0), grid_sample_pos, red_in, green_in, blue_in);
#endif

	const vec4 sh_basic_central_dir11 = vec4(0.28209481, 0.0, 0.48860252, 0.0);
	const vec4 vpl11 = vpl1;
	const float solid_angle11 = solid_angle3;

	const vec4 sh_basic_central_dir12 = vec4(0.28209481, 0.0, 0.41563013, -0.25687355);
	const vec4 vpl12 = vpl3;
	const float solid_angle12 =	solid_angle1;

	const vec4 sh_basic_central_dir13 = vec4(0.28209481, 0.0, 0.41563013, 0.25687355);
	const vec4 vpl13 = vpl8;
	const float solid_angle13 =	solid_angle1;

	const vec4 sh_basic_central_dir14 = vec4(0.28209481, 0.25687355, 0.41563013, 0.0);
	const vec4 vpl14 = vpl4;
	const float solid_angle14 = solid_angle1;

	const vec4 sh_basic_central_dir15 = vec4(0.28209481, -0.25687355, 0.41563013, 0.0);
	const vec4 vpl15 = vpl5;
	const float solid_angle15 = solid_angle1;

	// propagate light from adjacent cell to the five faces of this cell
	propagate(red_in, 
			  green_in, 
			  blue_in, 
			  

			  sh_basic_central_dir11,
			  vpl11,
			  solid_angle11,
			  sh_basic_central_dir12,
			  vpl12,
			  solid_angle12,
			  sh_basic_central_dir13,
			  vpl13,
			  solid_angle13,
			  sh_basic_central_dir14,
			  vpl14,
			  solid_angle14,
			  sh_basic_central_dir15,
			  vpl15,
			  solid_angle15,


			  vpl_r, 
			  vpl_g, 
			  vpl_b);

	s = step(0.0, sample_pos_neg.y);
	total_vpl_r += vpl_r * s;
	total_vpl_g += vpl_g * s;
	total_vpl_b += vpl_b * s;

	// get light intensity sh vectors from the cell behind this cell
	grid_sample_pos = vec3(tex_coord.xy, sample_pos_neg.z);
	get_vpls(grid_sample_pos, red_in, green_in, blue_in);
#ifndef NO_BLOCKING
	grid_sample_pos.z += half_texel_size; //sample pos of blocking potential is between both cells
	apply_blocking_potential(vec4(0.88622689, 1.0233266, 0.00000000, -0.00000000), grid_sample_pos, red_in, green_in, blue_in);
#endif


	const vec4 sh_basic_central_dir21 = vec4(0.28209481, 0.48860252, 0.0, 0.0);
	const vec4 vpl21 = vpl4;
	const float solid_angle21 =	solid_angle3;

	const vec4 sh_basic_central_dir22 = vec4(0.28209481, 0.41563013, 0.25687355, 0.0);
	const vec4 vpl22 = vpl1;
	const float solid_angle22 = solid_angle1;

	const vec4 sh_basic_central_dir23 = vec4(0.28209481, 0.41563013, -0.25687355, 0.0);
	const vec4 vpl23 = vpl2;
	const float	solid_angle23 = solid_angle1;

	const vec4 sh_basic_central_dir24 = vec4(0.28209481, 0.41563013, 0.0, 0.25687355);
	const vec4 vpl24 = vpl8;
	const float	solid_angle24 = solid_angle1;

	const vec4 sh_basic_central_dir25 = vec4(0.28209481, 0.41563013, 0.0, -0.25687355);
	const vec4 vpl25 = vpl3;
	const float solid_angle25 = solid_angle1;

	// propagate light from adjacent cell to the five faces of this cell
	propagate(red_in, 
			  green_in, 
			  blue_in, 
			  
			  sh_basic_central_dir21,
			  vpl21,
			  solid_angle21,
			  sh_basic_central_dir22,
			  vpl22,
			  solid_angle22,
			  sh_basic_central_dir23,
			  vpl23,
			  solid_angle23,
			  sh_basic_central_dir24,
			  vpl24,
			  solid_angle24,
			  sh_basic_central_dir25,
			  vpl25,
			  solid_angle25,


			  vpl_r, 
			  vpl_g, 
			  vpl_b);


	s = step(0.0, sample_pos_neg.z);
	total_vpl_r += vpl_r * s;
	total_vpl_g += vpl_g * s;
	total_vpl_b += vpl_b * s;

	// get light intensity sh vectors from the cell in front of this cell
	grid_sample_pos = vec3(tex_coord.xy, sample_pos_pos.z);
	get_vpls(grid_sample_pos, red_in, green_in, blue_in);
#ifndef NO_BLOCKING
	grid_sample_pos.z -= half_texel_size; //sample pos of blocking potential is between both cells
	apply_blocking_potential(vec4(0.88622689, -1.0233266, 0.00000000, -0.00000000), grid_sample_pos, red_in, green_in, blue_in);
#endif

	const vec4 sh_basic_central_dir26 = vec4(0.28209481, -0.48860252, 0.0, 0.0);
	const vec4 vpl26 = vpl5;
	const float	solid_angle26 = solid_angle3;

	const vec4 sh_basic_central_dir27 = vec4(0.28209481, -0.41563013, 0.25687355, 0.0);
	const vec4 vpl27 = vpl1;
	const float	solid_angle27 = solid_angle1;

	const vec4 sh_basic_central_dir28 = vec4(0.28209481, -0.41563013, -0.25687355, 0.0);
	const vec4 vpl28 = vec4(0.88622689, 0.0, -1.0233266, 0.0);
	const float	solid_angle28 = solid_angle1;

	const vec4 sh_basic_central_dir29 = vec4(0.28209481, -0.41563013, 0.0, 0.25687355);
	const vec4 vpl29 = vpl8;
	const float	solid_angle29 = solid_angle1;

	const vec4 sh_basic_central_dir30 = vec4(0.28209481, -0.41563013, 0.0, -0.25687355);
	const vec4 vpl30 = vpl3;
	const float	solid_angle30 = solid_angle1;
		
	// propagate light from adjacent cell to the five faces of this cell
	propagate(red_in, 
			  green_in, 
			  blue_in, 
			  
			  sh_basic_central_dir26,
			  vpl26,
			  solid_angle26,
			  sh_basic_central_dir27,
			  vpl27,
			  solid_angle27,
			  sh_basic_central_dir28,
			  vpl28,
			  solid_angle28,
			  sh_basic_central_dir29,
			  vpl29,
			  solid_angle29,
			  sh_basic_central_dir30,
			  vpl30,
			  solid_angle30,

			  vpl_r, 
			  vpl_g, 
			  vpl_b);


	s = step(sample_pos_pos.z, 1.0);
	total_vpl_r += vpl_r * s;
	total_vpl_g += vpl_g * s;
	total_vpl_b += vpl_b * s;

	// normalize
	const float pi = 3.141592;		
	total_vpl_r /= pi;
	total_vpl_g /= pi;
	total_vpl_b /= pi;

	// store light intensity as SH vectors
	gl_FragData[0] = total_vpl_r;
	gl_FragData[1] = total_vpl_g;
	gl_FragData[2] = total_vpl_b;
}
