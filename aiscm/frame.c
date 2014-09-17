#include <libswscale/swscale.h>
#include <libguile.h>

void scm_to_array(SCM source, int dest[])
{
  if (!scm_is_null_and_not_nil(source)) {
    *dest = scm_to_int(scm_car(source));
    scm_to_array(scm_cdr(source), dest + 1);
  };
}

void frame_setup(SCM scm_type, enum PixelFormat *format, int *width, int *height,
                 uint8_t *data[], int pitches[], void *ptr)
{
  int i;
  int offsets[8];
  *format = scm_to_int(scm_car(scm_type));
  *width = scm_to_int(scm_cadr(scm_type)),
  *height = scm_to_int(scm_caddr(scm_type));
  scm_to_array(scm_cadddr(scm_type), offsets);
  scm_to_array(scm_cadddr(scm_cdr(scm_type)), pitches);
  for (i=0; i<8; i++) data[i] = (uint8_t *)ptr + offsets[i];
}

SCM frame_convert(SCM scm_ptr, SCM scm_source_type, SCM scm_dest_ptr, SCM scm_dest_type)
{
  enum PixelFormat format;
  int width, height;
  void *ptr = scm_to_pointer(scm_ptr);
  uint8_t *source_data[8];
  int source_pitches[8];
  frame_setup(scm_source_type, &format, &width, &height, source_data, source_pitches, ptr);

  enum PixelFormat dest_format;
  int dest_width, dest_height;
  void *dest_ptr = scm_to_pointer(scm_dest_ptr);
  uint8_t *dest_data[8];
  int dest_pitches[8];
  frame_setup(scm_dest_type, &dest_format, &dest_width, &dest_height, dest_data, dest_pitches, dest_ptr);

  struct SwsContext *sws_context = sws_getContext(width, height, format,
                                                  dest_width, dest_height, dest_format,
                                                  SWS_FAST_BILINEAR, 0, 0, 0);
  sws_scale(sws_context, source_data, source_pitches, 0,
            height, dest_data, dest_pitches);

  sws_freeContext(sws_context);
  return SCM_UNDEFINED;
}

void init_frame(void)
{
  scm_c_define("PIX_FMT_RGB24",   scm_from_int(PIX_FMT_RGB24));
  scm_c_define("PIX_FMT_BGR24",   scm_from_int(PIX_FMT_BGR24));
  scm_c_define("PIX_FMT_BGRA",    scm_from_int(PIX_FMT_BGRA));
  scm_c_define("PIX_FMT_GRAY8",   scm_from_int(PIX_FMT_GRAY8));
  scm_c_define("PIX_FMT_YUV420P", scm_from_int(PIX_FMT_YUV420P));
  scm_c_define("PIX_FMT_UYVY422", scm_from_int(PIX_FMT_UYVY422));
  scm_c_define("PIX_FMT_YUYV422", scm_from_int(PIX_FMT_YUYV422));
  scm_c_define_gsubr("frame-convert", 4, 0, 0, frame_convert);
}
