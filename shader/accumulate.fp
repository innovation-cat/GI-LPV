// sh spectral coeffs
uniform sampler3D spectral_coeffs_r; 
uniform sampler3D spectral_coeffs_g; 
uniform sampler3D spectral_coeffs_b; 

varying vec3 tex_coord;

void main()
{
	gl_FragData[0] = texture3D(spectral_coeffs_r, tex_coord);
	gl_FragData[1] = texture3D(spectral_coeffs_g, tex_coord);
	gl_FragData[2] = texture3D(spectral_coeffs_b, tex_coord);	
}
