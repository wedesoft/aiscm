#ifndef __HELPERS_H
#define __HELPERS_H

#include <libguile.h>


SCM from_non_zero_array(int source[], int upto, int atleast);

void offsets_from_pointers(uint8_t *pointers[], int offsets[], int n);

void pack_audio(uint8_t *pointers[], int channels, int nb_samples, int data_size, uint8_t *destination);


struct ring_buffer_t
{
  void **buffer;
  int size;
  int count;
  int offset;
};

void ring_buffer_init(struct ring_buffer_t *ring_buffer, int size);

void ring_buffer_free(struct ring_buffer_t *ring_buffer);

void ring_buffer_push(struct ring_buffer_t *ring_buffer, void *element);

void *ring_buffer_pop(struct ring_buffer_t *ring_buffer);

int ring_buffer_full(struct ring_buffer_t *ring_buffer);

int ring_buffer_empty(struct ring_buffer_t *ring_buffer);

#endif
