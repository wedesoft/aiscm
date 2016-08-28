#include <libguile.h>


void init_ringbuffer_tests(void);
void init_ffmpeg_helpers_tests(void);

void init_tests(void)
{
  init_ringbuffer_tests();
  init_ffmpeg_helpers_tests();
}
