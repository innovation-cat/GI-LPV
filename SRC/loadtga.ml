
type image_data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Genarray.t;;
  
type image = {width:int; height:int; bpp:int; pixels:image_data};;

external load_image : string -> image = "load_image_c";;
