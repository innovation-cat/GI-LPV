open GL
open Glut

let win_width = ref 50
let win_height = ref 50
let display_mode = ref [GLUT_RGBA; GLUT_DEPTH; GLUT_DOUBLE]
let win_title = ref "GLobal_illumination"
let prev_frame = ref 0
let this_frame = ref 0
let frame_duration =ref  0
let version_major = ref 0
let version_minor = ref 0
let frame_counter = ref 0
let proj_mat_dirty = ref true
 

let reshape_func ~(width:int) ~(height:int) =
	proj_mat_dirty := true;
	win_width := width;
	win_height := height
;;

let init () = 
	ignore (glutInit Sys.argv);
	
	glutInitDisplayMode display_mode;
	
	glutInitWindowPosition ~x:100 ~y:100;

	glutInitWindowSize ~width:win_width ~height:win_height;

	glutCreateWindow ~title:win_title;

	glutReshapeFunc ~reshape:reshape_func;

	glutDisplayFunc ~display:display_func;

	glutIdleFunc ~idle:idle_func;
	
	glutKeyboardFunc ~keyboard:keyboard_func;

	glutKeyboardUpFunc ~keyboard_up:keyboard_up_func;

	glutSpecialFunc ~special:special_func;

	glutSpecialUpFunc ~special_up:special_up_func;
;;	

let main_loop = glutMainLoop ;;
