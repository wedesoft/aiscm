#include <stdlib.h>
#include <string.h>
#include "ringbuffer.h"

void ringbuffer_init(struct ringbuffer_t *ringbuffer, int size)
{
  ringbuffer->size = size;
  ringbuffer->buffer = malloc(size);
  ringbuffer_flush(ringbuffer);
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
    int boundary = ringbuffer->size - ringbuffer->read_offset;
    if (count >= boundary) {
      (*callback)(ringbuffer->buffer + ringbuffer->read_offset, boundary, userdata);
      (*callback)(ringbuffer->buffer, count - boundary, userdata);
      ringbuffer->read_offset = count - boundary;
    } else {
      (*callback)(ringbuffer->buffer + ringbuffer->read_offset, count, userdata);
      ringbuffer->read_offset += count;
    };
    ringbuffer->fill -= count;
  }
}

static void ringbuffer_copy_callback(char *data, int count, void *userdata)
{
  ringbuffer_store((struct ringbuffer_t *)userdata, data, count);
}

static void ringbuffer_resize(struct ringbuffer_t *ringbuffer, int size)
{
  struct ringbuffer_t resize;
  ringbuffer_init(&resize, size);
  ringbuffer_fetch(ringbuffer, ringbuffer->fill, ringbuffer_copy_callback, &resize);
  ringbuffer_destroy(ringbuffer);
  memcpy(ringbuffer, &resize, sizeof(struct ringbuffer_t));
}

void ringbuffer_store(struct ringbuffer_t *ringbuffer, const char *data, int count)
{
  if (ringbuffer->fill + count > ringbuffer->size) {
    ringbuffer_resize(ringbuffer, ringbuffer->fill + count + ringbuffer->size);
    ringbuffer_store(ringbuffer, data, count);
  } else {
    int boundary = ringbuffer->size - ringbuffer->write_offset;
    if (count >= boundary) {
      memcpy(ringbuffer->buffer + ringbuffer->write_offset, data, boundary);
      memcpy(ringbuffer->buffer, data + boundary, count - boundary);
      ringbuffer->write_offset = count - boundary;
    } else {
      memcpy(ringbuffer->buffer + ringbuffer->write_offset, data, count);
      ringbuffer->write_offset += count;
    };
    ringbuffer->fill += count;
  };
}

void ringbuffer_flush(struct ringbuffer_t *ringbuffer)
{
  ringbuffer->fill = 0;
  ringbuffer->read_offset = 0;
  ringbuffer->write_offset = 0;
}
