#include <stdlib.h>
#include <string.h>
#include "ringbuffer.h"

void ringbuffer_init(struct ringbuffer_t *ringbuffer, int size)
{
  ringbuffer->fill = 0;
  ringbuffer->offset = 0;
  ringbuffer->size = size;
  ringbuffer->buffer = malloc(size);
}

void ringbuffer_destroy(struct ringbuffer_t *ringbuffer)
{
  free(ringbuffer->buffer);
}

void ringbuffer_fetch(struct ringbuffer_t *ringbuffer, int size, ringbuffer_callback_t callback, void *userdata)
{
  int count = size < ringbuffer->fill ? size : ringbuffer->fill;
  int startpos = ringbuffer->offset;
  int boundary = ringbuffer->size - startpos;
  if (size > boundary) {
    (*callback)(ringbuffer->buffer + startpos, boundary, userdata);
    (*callback)(ringbuffer->buffer, count - boundary, userdata);
  } else
    (*callback)(ringbuffer->buffer + ringbuffer->offset, count, userdata);
  ringbuffer->offset += count;
  ringbuffer->fill -= count;
}

void ringbuffer_store(struct ringbuffer_t *ringbuffer, const char *data, int count)
{
  int startpos = ringbuffer->offset + ringbuffer->fill;// TODO: resize buffer
  int boundary = ringbuffer->size - startpos;
  if (count > boundary) {
    memcpy(ringbuffer->buffer + startpos, data, boundary);
    memcpy(ringbuffer->buffer, data + boundary, count - boundary);
  } else
    memcpy(ringbuffer->buffer + startpos, data, count);
  ringbuffer->fill += count;
}
