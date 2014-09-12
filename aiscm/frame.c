#include <libswscale/swscale.h>
#include <libguile.h>

static void setup_format(enum PixelFormat format, int width, int height, void *ptr,
                         uint8_t *data[], int line_size[])
{
  switch (format) {// TODO: YV12 and I420 have different packing
    case PIX_FMT_RGB24:
    case PIX_FMT_BGR24:
      data[0] = (uint8_t *)ptr;
      line_size[0] = width * 3;
      break;
    case PIX_FMT_BGRA:
      data[0] = (uint8_t *)ptr;
      line_size[0] = width * 4;
      break;
    case PIX_FMT_GRAY8:
      data[0] = (uint8_t *)ptr;
      line_size[0] = width;
      break;
    case PIX_FMT_YUV420P:
      data[0] = (uint8_t *)ptr;
      data[1] = (uint8_t *)ptr + width * height;
      data[2] = (uint8_t *)data[1] + ((width + 1) >> 1) * ((height + 1) >> 1);
      line_size[0] = width;
      line_size[1] = (width + 1) >> 1;
      line_size[2] = (width + 1) >> 1;
      break;
    case PIX_FMT_UYVY422:
    case PIX_FMT_YUYV422:
      data[0] = (uint8_t *)ptr;
      line_size[0] = 2 * ((width + 3) & ~0x3);
      break;
    default:
      scm_misc_error("setup_format", "Support for format ~a not implemented",
                     scm_list_1(scm_from_int(format)));
  };
}

static int frame_size(enum PixelFormat format, int width, int height)
{
  int retval;
  switch (format) {
    case PIX_FMT_RGB24:
    case PIX_FMT_BGR24:
      retval = width * height * 3;
      break;
    case PIX_FMT_BGRA:
      retval = width * height * 4;
      break;
    case PIX_FMT_GRAY8:
      retval = width * height;
      break;
    case PIX_FMT_YUV420P:
      retval = width * height + 2 * ((width + 1) >> 1) * ((height + 1) >> 1);
      break;
    case PIX_FMT_UYVY422:
    case PIX_FMT_YUYV422:
      retval = ((width + 3) & ~0x3) * height * 2;
      break;
    default:
      scm_misc_error("setup_format", "Support for format ~a not implemented",
                     scm_list_1(scm_from_int(format)));
  };
  return retval;
}

SCM frame_convert(SCM scm_format, SCM scm_width, SCM scm_height, SCM scm_ptr,
                  SCM scm_dest_format, SCM scm_dest_width, SCM scm_dest_height)
{
  enum PixelFormat format = scm_to_int(scm_format);
  int
    width = scm_to_int(scm_width),
    height = scm_to_int(scm_height);
  void *ptr = scm_to_pointer(scm_ptr);
  uint8_t *source_data[8];
  int source_line_size[8];
  setup_format(format, width, height, ptr, source_data, source_line_size);
  enum PixelFormat dest_format = scm_to_int(scm_dest_format);
  int
    dest_width = scm_to_int(scm_dest_width),
    dest_height = scm_to_int(scm_dest_height);
  void *dest_ptr = scm_gc_malloc_pointerless(frame_size(dest_format, dest_width, dest_height), "frame");
  uint8_t *dest_data[8];
  int dest_line_size[8];
  setup_format(dest_format, dest_width, dest_height, dest_ptr, dest_data, dest_line_size);
  struct SwsContext *sws_context = sws_getContext(width, height, format,
                                                  dest_width, dest_height, dest_format,
                                                  SWS_FAST_BILINEAR, 0, 0, 0);
  sws_scale(sws_context, source_data, source_line_size, 0,
            height, dest_data, dest_line_size);
  sws_freeContext(sws_context);
  return scm_from_pointer(dest_ptr, NULL);
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
  scm_c_define_gsubr("frame-convert", 7, 0, 0, frame_convert);
}
