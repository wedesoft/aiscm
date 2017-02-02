// AIscm - Guile extension for numerical arrays and tensors.
// Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Wedekind <jan@wedesoft.de>
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
#ifndef __AISCM_RINGBUFFER_H
#define __AISCM_RINGBUFFER_H

struct ringbuffer_t {
  int fill;
  int read_offset;
  int write_offset;
  int size;
  char *buffer;
};

typedef void (*ringbuffer_callback_t)(char *data, int count, void *userdata);

void ringbuffer_init(struct ringbuffer_t *ringbuffer, int size);

void ringbuffer_destroy(struct ringbuffer_t *ringbuffer);

void ringbuffer_fetch(struct ringbuffer_t *ringbuffer, int count, ringbuffer_callback_t callback, void *userdata);

void ringbuffer_store(struct ringbuffer_t *ringbuffer, const char *data, int count);

void ringbuffer_flush(struct ringbuffer_t *ringbuffer);

#endif
