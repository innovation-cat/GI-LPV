## GI-LPV ##
Implement indirect illumination using "light propagation volume" technique with OCaml, a strong static functional programming language.

In this project, on one hand, I want to consolidate my OCaml programming skill, on the other hand, I try to build a large project which integrates OCaml with GPU programming.
 
if you have any comments or suggestions, please feel free to let me know.

## Prerequisites ##
1. Make sure you have installed OCaml (4.0) and glMLite, glMLite is a package which provide OpenGL bindings for  OCaml, for more detail about that, please refer to [glMLite]( http://www.linux-nantes.org/~fmonnier/ocaml/GL/index.html "glMLite")

2. Make sure your graphic card support OpenGL 3.0 with **EXT _ GPU _ shader4** extension.


## Abstract ##
In this project, I implement the following tasks:

1. generate reflect shadow map (RSM)
2. inject rsm info to volume textures
3. propagation
4. apply volume textures to scene rending