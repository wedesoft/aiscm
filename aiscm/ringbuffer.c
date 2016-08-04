#include "ringbuffer.h"

void ringbuffer_init(struct ringbuffer_t *ringbuffer, int size)
{
  ringbuffer->fill = 0;
  ringbuffer->size = size;
}

void ringbuffer_destroy(struct ringbuffer_t *ringbuffer)
{
}

void ringbuffer_store(struct ringbuffer_t *ringbuffer, void *data, int n)
{
  ringbuffer->fill += n;
}
