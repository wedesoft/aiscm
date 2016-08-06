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

void ringbuffer_fetch(struct ringbuffer_t *ringbuffer, int count, ringbuffer_callback_t callback, void *userdata)
{
  if (count > ringbuffer->fill)
    ringbuffer_fetch(ringbuffer, ringbuffer->fill, callback, userdata);
  else {
    int startpos = ringbuffer->offset;
    int boundary = ringbuffer->size - startpos;
    if (count > boundary) {
      (*callback)(ringbuffer->buffer + startpos, boundary, userdata);
      (*callback)(ringbuffer->buffer, count - boundary, userdata);
    } else
      (*callback)(ringbuffer->buffer + startpos, count, userdata);
    ringbuffer->offset += count;// TODO: wrap around
    ringbuffer->fill -= count;
  }
}

void ringbuffer_copy_callback(char *data, int count, void *userdata)
{
  ringbuffer_store((struct ringbuffer_t *)userdata, data, count);
}

void ringbuffer_store(struct ringbuffer_t *ringbuffer, const char *data, int count)
{
  if (ringbuffer->fill + count > ringbuffer->size) {
    struct ringbuffer_t resize;
    ringbuffer_init(&resize, ringbuffer->fill + count);
    ringbuffer_fetch(ringbuffer, ringbuffer->fill, ringbuffer_copy_callback, &resize);
    ringbuffer_destroy(ringbuffer);
    memcpy(ringbuffer, &resize, sizeof(struct ringbuffer_t));
    ringbuffer_store(ringbuffer, data, count);
  } else {
    int startpos = ringbuffer->offset + ringbuffer->fill;
    int boundary = ringbuffer->size - startpos;
    if (count > boundary) {
      memcpy(ringbuffer->buffer + startpos, data, boundary);
      memcpy(ringbuffer->buffer, data + boundary, count - boundary);
    } else
      memcpy(ringbuffer->buffer + startpos, data, count);
    ringbuffer->fill += count;
  };
}
