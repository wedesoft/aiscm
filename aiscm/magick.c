#include <libguile.h>
#include <magick/MagickCore.h>

SCM magick_read_image(SCM scm_fileName)
{
  SCM retval;
  retval = scm_from_int(1234);
  return retval;
}

void init_magick(void)
{
  scm_c_define_gsubr("magick-read-image", 1, 0, 0, magick_read_image);
}
