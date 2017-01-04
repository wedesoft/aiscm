// AIscm - Guile extension for numerical arrays and tensors.
// Copyright (C) 2013, 2014, 2015, 2016 Jan Wedekind <jan@wedesoft.de>
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
#include <libguile.h>
#include "ringbuffer.h"


static void test_empty_callback(char *data, int count, void *userdata)
{
  *(char *)userdata = count == 0;
}

SCM ringbuffer_fetch_empty(void)
{
  struct ringbuffer_t ringbuffer;
  ringbuffer_init(&ringbuffer, 1024);
  char retval = 0;
  ringbuffer_fetch(&ringbuffer, 123, test_empty_callback, &retval);
  ringbuffer_destroy(&ringbuffer);
  return scm_from_bool(retval);
}

SCM ringbuffer_initial_size(void)
{
  struct ringbuffer_t ringbuffer;
  ringbuffer_init(&ringbuffer, 1024);
  SCM retval = scm_from_bool(ringbuffer.size == 1024);
  ringbuffer_destroy(&ringbuffer);
  return retval;
}

SCM ringbuffer_add_data(void)
{
  struct ringbuffer_t ringbuffer;
  ringbuffer_init(&ringbuffer, 1024);
  char data[100];
  ringbuffer_store(&ringbuffer, data, 100);
  SCM retval = scm_from_bool(ringbuffer.fill == 100);
  ringbuffer_destroy(&ringbuffer);
  return retval;
}

static void test_fetch_callback(char *data, int count, void *userdata)
{
  *(char *)userdata = count == 5 && strcmp(data, "test") == 0;
}

SCM ringbuffer_store_and_fetch(void)
{
  struct ringbuffer_t ringbuffer;
  ringbuffer_init(&ringbuffer, 1024);
  ringbuffer_store(&ringbuffer, "test", 5);
  char retval = 0;
  ringbuffer_fetch(&ringbuffer, 123, test_fetch_callback, &retval);
  ringbuffer_destroy(&ringbuffer);
  return scm_from_bool(retval);
}

static void test_append_callback(char *data, int count, void *userdata)
{
  *(char *)userdata = count == 9 && strcmp(data, "testmore") == 0;
}

SCM ringbuffer_store_appends_data(void)
{
  struct ringbuffer_t ringbuffer;
  ringbuffer_init(&ringbuffer, 1024);
  ringbuffer_store(&ringbuffer, "test", 4);
  ringbuffer_store(&ringbuffer, "more", 5);
  char retval = 0;
  ringbuffer_fetch(&ringbuffer, 123, test_append_callback, &retval);
  ringbuffer_destroy(&ringbuffer);
  return scm_from_bool(retval);
}

static void test_limit_callback(char *data, int count, void *userdata)
{
  *(char *)userdata = count == 4 && strncmp(data, "test", 4) == 0;
}

SCM ringbuffer_fetch_limit(void)
{
  struct ringbuffer_t ringbuffer;
  ringbuffer_init(&ringbuffer, 1024);
  ringbuffer_store(&ringbuffer, "testmore", 9);
  char retval = 0;
  ringbuffer_fetch(&ringbuffer, 4, test_limit_callback, &retval);
  ringbuffer_destroy(&ringbuffer);
  return scm_from_bool(retval);
}

static void test_advances_callback(char *data, int count, void *userdata)
{
  *(char *)userdata = count == 4 && strncmp(data, "more", 4) == 0;
}

SCM ringbuffer_fetching_advances(void)
{
  struct ringbuffer_t ringbuffer;
  ringbuffer_init(&ringbuffer, 1024);
  ringbuffer_store(&ringbuffer, "testmore", 9);
  char retval = 0;
  ringbuffer_fetch(&ringbuffer, 4, test_advances_callback, &retval);
  ringbuffer_fetch(&ringbuffer, 4, test_advances_callback, &retval);
  ringbuffer_destroy(&ringbuffer);
  return scm_from_bool(retval);
}

static void test_offset_callback(char *data, int count, void *userdata)
{
  *(char *)userdata = count == 4 && strncmp(data, "cdef", 4) == 0;
}

SCM ringbuffer_storing_respects_offset(void)
{
  struct ringbuffer_t ringbuffer;
  ringbuffer_init(&ringbuffer, 1024);
  char retval = 0;
  ringbuffer_store(&ringbuffer, "abcd", 4);
  ringbuffer_fetch(&ringbuffer, 2, test_offset_callback, &retval);
  ringbuffer_store(&ringbuffer, "ef", 3);
  ringbuffer_fetch(&ringbuffer, 4, test_offset_callback, &retval);
  ringbuffer_destroy(&ringbuffer);
  return scm_from_bool(retval);
}

static void test_wrap_callback(char *data, int count, void *userdata)
{
  strncat((char *)userdata, data, count);
}

SCM ringbuffer_wrap_around(void)
{
  struct ringbuffer_t ringbuffer;
  ringbuffer_init(&ringbuffer, 4);
  char buf[7];
  buf[0] = '\0';
  ringbuffer_store(&ringbuffer, "abcd", 4);
  ringbuffer_fetch(&ringbuffer, 2, test_wrap_callback, buf);
  ringbuffer_store(&ringbuffer, "ef", 2);
  ringbuffer_fetch(&ringbuffer, 4, test_wrap_callback, buf);
  SCM retval = scm_from_bool(!strncmp(ringbuffer.buffer, "efcd", 4) &&
                             !strcmp(buf, "abcdef") &&
                             ringbuffer.read_offset == 2);
  ringbuffer_destroy(&ringbuffer);
  return retval;
}

static void test_grow_callback(char *data, int count, void *userdata)
{
  *(char *)userdata = count == 8 && strncmp(data, "abcdefgh", 8) == 0;
}

SCM ringbuffer_grow(void)
{
  struct ringbuffer_t ringbuffer;
  ringbuffer_init(&ringbuffer, 4);
  ringbuffer_store(&ringbuffer, "abcdefgh", 8);
  char retval = 0;
  ringbuffer_fetch(&ringbuffer, 8, test_grow_callback, &retval);
  ringbuffer_destroy(&ringbuffer);
  return scm_from_bool(retval);
}

static void test_wrap_write_callback(char *data, int count, void *userdata)
{
  *(char *)userdata = count == 2 && strncmp(data, "ef", 2) == 0;
}

SCM ringbuffer_wrap_write(void)
{
  struct ringbuffer_t ringbuffer;
  ringbuffer_init(&ringbuffer, 4);
  ringbuffer_store(&ringbuffer, "abc", 3);
  char retval = 0;
  ringbuffer_fetch(&ringbuffer, 2, test_wrap_write_callback, &retval);
  ringbuffer_store(&ringbuffer, "de", 2);
  ringbuffer_store(&ringbuffer, "f", 1);
  ringbuffer_fetch(&ringbuffer, 2, test_wrap_write_callback, &retval);
  ringbuffer_fetch(&ringbuffer, 2, test_wrap_write_callback, &retval);
  ringbuffer_destroy(&ringbuffer);
  return scm_from_bool(retval);
}

static void test_flushing(char *data, int count, void *userdata)
{
  *(char *)userdata = count == 2 && strncmp(data, "cd", 2) == 0;
}

SCM ringbuffer_flushing(void)
{
  struct ringbuffer_t ringbuffer;
  ringbuffer_init(&ringbuffer, 4);
  ringbuffer_store(&ringbuffer, "ab", 2);
  ringbuffer_flush(&ringbuffer);
  ringbuffer_store(&ringbuffer, "cd", 2);
  char retval = 0;
  ringbuffer_fetch(&ringbuffer, 2, test_flushing, &retval);
  ringbuffer_destroy(&ringbuffer);
  return scm_from_bool(retval);
}

void init_ringbuffer_tests(void)
{
  scm_c_define_gsubr("ringbuffer-fetch-empty", 0, 0, 0, ringbuffer_fetch_empty);
  scm_c_define_gsubr("ringbuffer-initial-size", 0, 0, 0, ringbuffer_initial_size);
  scm_c_define_gsubr("ringbuffer-add-data", 0, 0, 0, ringbuffer_add_data);
  scm_c_define_gsubr("ringbuffer-store-and-fetch", 0, 0, 0, ringbuffer_store_and_fetch);
  scm_c_define_gsubr("ringbuffer-store-appends-data", 0, 0, 0, ringbuffer_store_appends_data);
  scm_c_define_gsubr("ringbuffer-fetch-limit", 0, 0, 0, ringbuffer_fetch_limit);
  scm_c_define_gsubr("ringbuffer-fetching-advances", 0, 0, 0, ringbuffer_fetching_advances);
  scm_c_define_gsubr("ringbuffer-storing-respects-offset", 0, 0, 0, ringbuffer_storing_respects_offset);
  scm_c_define_gsubr("ringbuffer-wrap-around", 0, 0, 0, ringbuffer_wrap_around);
  scm_c_define_gsubr("ringbuffer-grow", 0, 0, 0, ringbuffer_grow);
  scm_c_define_gsubr("ringbuffer-wrap-write", 0, 0, 0, ringbuffer_wrap_write);
  scm_c_define_gsubr("ringbuffer-flushing", 0, 0, 0, ringbuffer_flushing);
}

