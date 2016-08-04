#include "ringbuffer.h"

void ringbuffer_init(struct ringbuffer_t *ringbuffer, int size)
{
  ringbuffer->size = 0;
}

void ringbuffer_destroy(struct ringbuffer_t *ringbuffer)
{
}
