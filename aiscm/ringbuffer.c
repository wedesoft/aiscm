// AIscm - Guile extension for numerical arrays and tensors.
// Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018, 2019 Jan Wedekind <jan@wedesoft.de>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
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
      (*callback)(ringbuffer->buffer + ringbuffer->read_offset, boundary, 0, userdata);
      (*callback)(ringbuffer->buffer, count - boundary, boundary, userdata);
      ringbuffer->read_offset = count - boundary;
    } else {
      (*callback)(ringbuffer->buffer + ringbuffer->read_offset, count, 0, userdata);
      ringbuffer->read_offset += count;
    };
    ringbuffer->fill -= count;
  }
}

static void ringbuffer_copy_callback(char *data, int count, int offset, void *userdata)
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
