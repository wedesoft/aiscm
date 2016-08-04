#ifndef __AISCM_RINGBUFFER_H
#define __AISCM_RINGBUFFER_H

struct ringbuffer_t {
  int fill;
  int size;
  char *buffer;
};

void ringbuffer_init(struct ringbuffer_t *ringbuffer, int size);

void ringbuffer_destroy(struct ringbuffer_t *ringbuffer);

void ringbuffer_store(struct ringbuffer_t *ringbuffer, const char *data, int n);

#endif
