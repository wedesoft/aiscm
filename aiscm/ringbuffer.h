#ifndef __AISCM_RINGBUFFER_H
#define __AISCM_RINGBUFFER_H

struct ringbuffer_t {
  int size;
};

void ringbuffer_init(struct ringbuffer_t *ringbuffer, int size);

void ringbuffer_destroy(struct ringbuffer_t *ringbuffer);

#endif
