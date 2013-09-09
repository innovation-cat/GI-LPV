
open GL

type internal_format =
  | GL_ALPHA
  | GL_ALPHA4
  | GL_ALPHA8
  | GL_ALPHA12
  | GL_ALPHA16
  | GL_COMPRESSED_ALPHA
  | GL_COMPRESSED_LUMINANCE
  | GL_COMPRESSED_LUMINANCE_ALPHA
  | GL_COMPRESSED_INTENSITY
  | GL_COMPRESSED_RGB
  | GL_COMPRESSED_RGBA
  | GL_DEPTH_COMPONENT
  | GL_DEPTH_COMPONENT16
  | GL_DEPTH_COMPONENT24
  | GL_DEPTH_COMPONENT32
  | GL_LUMINANCE
  | GL_LUMINANCE4
  | GL_LUMINANCE8
  | GL_LUMINANCE12
  | GL_LUMINANCE16
  | GL_LUMINANCE_ALPHA
  | GL_LUMINANCE4_ALPHA4
  | GL_LUMINANCE6_ALPHA2
  | GL_LUMINANCE8_ALPHA8
  | GL_LUMINANCE12_ALPHA4
  | GL_LUMINANCE12_ALPHA12
  | GL_LUMINANCE16_ALPHA16
  | GL_INTENSITY
  | GL_INTENSITY4
  | GL_INTENSITY8
  | GL_INTENSITY12
  | GL_INTENSITY16
  | GL_R3_G3_B2
  | GL_RGB
  | GL_RGB4
  | GL_RGB5
  | GL_RGB8
  | GL_RGB10
  | GL_RGB12
  | GL_RGB16
  | GL_RGBA
  | GL_RGBA2
  | GL_RGBA4
  | GL_RGB5_A1
  | GL_RGBA8
  | GL_RGB10_A2
  | GL_RGBA12
  | GL_RGBA16
  | GL_SLUMINANCE
  | GL_SLUMINANCE8
  | GL_SLUMINANCE_ALPHA
  | GL_SLUMINANCE8_ALPHA8
  | GL_SRGB
  | GL_SRGB8
  | GL_SRGB_ALPHA
  | GL_SRGB8_ALPHA8
  | GL_RG16F
  | GL_R16F


external glShaderSources : GL.shader_object -> int -> string array option -> int array option -> unit = "ml_glshadersources_ex"

external glTexImage2D: GL.TexTarget.target_2d -> int -> internal_format -> int -> int -> GL.pixel_data_format -> GL.pixel_data_type ->       image_data option -> unit = "ml_glteximage2d_ex_bytecode" "ml_glteximage2d_ex_native" "noalloc"

external glGenerateMipmapEXT : GL.BindTex.texture_binding -> unit = "ml_glgeneratemipmapext_ex"
