#ifndef __AISCM_RINGBUFFER_H
#define __AISCM_RINGBUFFER_H

struct ringbuffer_t {
  int fill;
  int offset;
  int size;
  char *buffer;
};

typedef void (*ringbuffer_callback_t)(char *data, int size, void *userdata);

void ringbuffer_init(struct ringbuffer_t *ringbuffer, int size);

void ringbuffer_destroy(struct ringbuffer_t *ringbuffer);

void ringbuffer_fetch(struct ringbuffer_t *ringbuffer, int size, ringbuffer_callback_t callback, void *userdata);

void ringbuffer_store(struct ringbuffer_t *ringbuffer, const char *data, int n);

#endif
