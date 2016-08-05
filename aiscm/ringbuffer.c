#include <stdlib.h>
#include <string.h>
#include "ringbuffer.h"

void ringbuffer_init(struct ringbuffer_t *ringbuffer, int size)
{
  ringbuffer->fill = 0;
  ringbuffer->size = size;
  ringbuffer->buffer = malloc(size);
}

void ringbuffer_destroy(struct ringbuffer_t *ringbuffer)
{
  free(ringbuffer->buffer);
}

void ringbuffer_fetch(struct ringbuffer_t *ringbuffer, int size, ringbuffer_callback_t callback, void *userdata)
{
  
}

void ringbuffer_store(struct ringbuffer_t *ringbuffer, const char *data, int n)
{
  memcpy(ringbuffer->buffer + ringbuffer->fill, data, n);
  ringbuffer->fill += n;
}
