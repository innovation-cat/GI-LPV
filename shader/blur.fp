// pixel shader

uniform sampler2D depth_normal_tex;
uniform sampler2D indirect_light_tex;
uniform vec2 near_far_plane;
uniform mat4 view_to_grid_mat;
uniform vec2 inv_proj;
uniform vec2 grid_to_texture_scale; 
uniform vec4 dist_threashold;

varying vec2 tex_coord;
varying vec2 clip_pos;

const float weight1 = 0.05449;
const float weight2 = 0.24420;
const float center_weight = 0.40262;

// get depth in view space
float unpack_depth(in vec2 d)
{
	float depth = d.x + d.y / 255.0;
	depth = depth * near_far_plane.y + near_far_plane.x;
	return depth;
}

vec3 unpack_normal(in vec2 n)
{
	vec3 normal;
	normal.xy = 2.0 * (n - 0.5);
	vec2 n2 = normal.xy * normal.xy;
	float z2 = abs(1.0 - n2.x - n2.y);
	normal.z = sqrt(z2);
	return normal;
}

// calculate position in view space from view space depth
vec3 calc_view_pos(in float depth, in vec2 pos)
{
	vec3 view_pos = vec3(pos, -1.0);
	view_pos.xy = view_pos.xy * inv_proj.xy; 
	view_pos *= depth;

	return view_pos;
}

// read surface normal, position and indirect light
void get_sample(in vec2 offset, out vec3 pos, out vec3 normal, out vec3 indirect_light)
{
	vec2 tex_coord2 = offset + tex_coord;
	vec4 t = texture2D(depth_normal_tex, tex_coord2);

	vec2 clip_pos2 = tex_coord2 * 2.0 - 1.0;
	float depth = unpack_depth(t.rg);
	pos = calc_view_pos(depth, clip_pos2);

	indirect_light = texture2D(indirect_light_tex, tex_coord2).rgb;

	normal = unpack_normal(t.ba);
}

vec4 contribution(vec3 center, vec3 pos1, vec3 pos2, vec3 pos3, vec3 pos4, vec3 center_normal, vec3 normal1, vec3 normal2, vec3 normal3, vec3 normal4)
{
	// check if the neighbour is on the same surface by comparing surface normal and position
	const float min_cos = 0.95;

	vec3 v1 = center - pos1;
	vec3 v2 = center - pos2;
	vec3 v3 = center - pos3;
	vec3 v4 = center - pos4;

	vec4 dist = vec4(dot(v1, v1), dot(v2, v2), dot(v3, v3), dot(v4, v4) );
	vec4 angle = vec4(dot(center_normal, normal1), dot(center_normal, normal2), dot(center_normal, normal3), dot(center_normal, normal4) );

	// return 0 or 1
	// 1 if neighbour is on the same surface
	return step(dist, dist_threashold) * step(min_cos, angle);
}

void main()
{	
	// use a bilateral filter to blur indirect light values

	// read surface normal and depth
	vec4 t = texture2D(depth_normal_tex, tex_coord);
	float depth = unpack_depth(t.rg);
	vec3 pos = calc_view_pos(depth, clip_pos.xy);
	// read corresponding indirect light value
	vec3 indirect_light = texture2D(indirect_light_tex, tex_coord).rgb;
	vec3 normal = unpack_normal(t.ba);

	// project the distance between cell centers in world space to texture space
	vec2 d = grid_to_texture_scale / vec2(depth);

	vec2 sample_pos2 = d * 2.0;
	vec2 sample_pos3 = d;
	vec2 sample_pos4 = -sample_pos3;
	vec2 sample_pos5 = -sample_pos2;

	// get values of neighbours
	vec3 indirect_light2;
	vec3 pos2;
	vec3 normal2;
	get_sample(sample_pos2, pos2, normal2, indirect_light2);

	vec3 indirect_light3;
	vec3 pos3;
	vec3 normal3;
	get_sample(sample_pos3, pos3, normal3, indirect_light3);

	vec3 indirect_light4;
	vec3 pos4;
	vec3 normal4;
	get_sample(sample_pos4, pos4, normal4, indirect_light4);

	vec3 indirect_light5;
	vec3 pos5;
	vec3 normal5;
	get_sample(sample_pos5, pos5, normal5, indirect_light5);

	// blur

	// center
	vec3 indirect = indirect_light * center_weight;

	// neighbours
	vec4 contribution_values = contribution(pos, pos2, pos3, pos4, pos5, normal, normal2, normal3, normal4, normal5);
	indirect += mix(indirect_light * weight1, indirect_light2 * weight1, contribution_values.x);
	indirect += mix(indirect_light * weight2, indirect_light3 * weight2, contribution_values.y);
	indirect += mix(indirect_light * weight2, indirect_light4 * weight2, contribution_values.z);
	indirect += mix(indirect_light * weight1, indirect_light5 * weight1, contribution_values.w);


	gl_FragColor.rgb = indirect;
}