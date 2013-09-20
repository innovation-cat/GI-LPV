uniform sampler3D gv_from_rsm; 
uniform sampler3D gv_from_visible_surface; 

varying vec3 tex_coord;

void main()
{
	// if we have two blocking potentials in the same cell we have to select one
	
	// select based on magnitude
	vec4 rsm_blocker = texture3D(gv_from_rsm, tex_coord);
	float rsm_blocker_mag = dot(rsm_blocker, rsm_blocker);

	vec4 surface_blocker = texture3D(gv_from_visible_surface, tex_coord);
	float surface_blocker_mag = dot(surface_blocker, surface_blocker);

	// check if rsm blocker is visible, surface blocker is visible because it comes from a surface in view space
	float angle = dot(rsm_blocker, surface_blocker);

	vec4 blocker = rsm_blocker_mag > surface_blocker_mag ? rsm_blocker : surface_blocker;
	blocker = angle > 0.0 ? blocker : surface_blocker;

	gl_FragColor = blocker;
}
