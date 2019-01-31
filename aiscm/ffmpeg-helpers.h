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
#pragma once
#include <libguile.h>


void int_array_to_long(int64_t destination[], int32_t source[], int n);

SCM from_non_zero_array(int64_t source[], int upto, int atleast);

void offsets_from_pointers(uint8_t *pointers[], int64_t offsets[], int n);

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
