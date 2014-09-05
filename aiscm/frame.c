#include <libavutil/pixfmt.h>
#include <libguile.h>

void init_frame(void)
{
  scm_c_define("PIX_FMT_GRAY8", scm_from_int(PIX_FMT_GRAY8));
}
