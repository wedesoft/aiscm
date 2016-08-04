#ifndef __AISCM_RINGBUFFER_H
#define __AISCM_RINGBUFFER_H

struct ringbuffer_t {
  int fill;
  int size;
};

void ringbuffer_init(struct ringbuffer_t *ringbuffer, int size);

void ringbuffer_destroy(struct ringbuffer_t *ringbuffer);

void ringbuffer_store(struct ringbuffer_t *ringbuffer, void *data, int n);

#endif
