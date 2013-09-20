// pixel shader

varying vec4 blocking_potential;

void main()
{	
	gl_FragColor = blocking_potential;
}
