#include <libguile.h>
#include "ffmpeg-helpers.h"
#include "ringbuffer.h"


SCM forty_two(void)
{
  return scm_from_int(42);
}

SCM from_array_empty(void)
{
  return from_non_zero_array(NULL, 0, 0);
}

SCM from_array_three_elements(void)
{
  int array[3] = {2, 3, 5};
  return from_non_zero_array(array, 3, 1);
}

SCM from_array_stop_at_zero(void)
{
  int array[5] = {2, 3, 5, 0, 7};
  return from_non_zero_array(array, 5, 1);
}

SCM from_array_at_least_one(void)
{
  int array[3] = {0, 0, 0};
  return from_non_zero_array(array, 3, 1);
}

SCM first_offset_is_zero(void)
{
  uint8_t buffer[1024];
  uint8_t *data[1] = {buffer};
  int offsets[1];
  offsets_from_pointers(data, offsets, 1);
  return scm_from_bool(offsets[0] == 0);
}

SCM second_offset_correct(void)
{
  uint8_t buffer[1024];
  uint8_t *data[2] = {buffer, buffer + 12};
  int offsets[2];
  offsets_from_pointers(data, offsets, 2);
  return scm_from_bool(offsets[1] == 12);
}

SCM zero_offset_for_null_pointer(void)
{
  uint8_t buffer[1024];
  uint8_t *data[2] = {buffer, NULL};
  int offsets[2];
  offsets_from_pointers(data, offsets, 2);
  return scm_from_bool(offsets[1] == 0);
}

SCM pack_byte_audio_sample(void)
{
  uint8_t a[1] = {1};
  uint8_t b[1] = {2};
  uint8_t *data[2] = {a, b};
  uint8_t destination[2];
  pack_audio(data, 2, 1, sizeof(uint8_t), destination);
  return scm_from_bool(destination[0] == 1 && destination[1] == 2);
}

SCM pack_byte_audio_samples(void)
{
  uint8_t a[3] = {1, 3, 5};
  uint8_t b[3] = {2, 4, 6};
  uint8_t *data[2] = {a, b};
  uint8_t destination[6];
  pack_audio(data, 2, 3, sizeof(uint8_t), destination);
  return scm_from_bool(destination[4] == 5 && destination[5] == 6);
}

SCM pack_short_int_audio_samples(void)
{
  int16_t a[3] = {1, 3, 5};
  int16_t b[3] = {2, 4, 6};
  uint8_t *data[2] = {(uint8_t *)a, (uint8_t *)b};
  uint16_t destination[6];
  pack_audio(data, 2, 3, sizeof(uint16_t), (uint8_t *)destination);
  return scm_from_bool(destination[4] == 5 && destination[5] == 6);
}

void test_empty_callback(char *data, int count, void *userdata)
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

void test_fetch_callback(char *data, int count, void *userdata)
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

void test_append_callback(char *data, int count, void *userdata)
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

void test_limit_callback(char *data, int count, void *userdata)
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

void test_advances_callback(char *data, int count, void *userdata)
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

void test_offset_callback(char *data, int count, void *userdata)
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

void test_wrap_callback(char *data, int count, void *userdata)
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
  return scm_from_bool(!strncmp(ringbuffer.buffer, "efcd", 4) && !strcmp(buf, "abcdef"));
}

void test_grow_callback(char *data, int count, void *userdata)
{
  printf("%s\n", data);
  *(char *)userdata = count == 8 && strncmp(data, "abcdefgh", 8) == 0;
}

SCM ringbuffer_grow(void)
{
  struct ringbuffer_t ringbuffer;
  ringbuffer_init(&ringbuffer, 4);
  ringbuffer_store(&ringbuffer, "abcdefgh", 8);
  char retval = 0;
  ringbuffer_fetch(&ringbuffer, 8, test_grow_callback, &retval);
  return scm_from_bool(retval);
}

void init_tests(void)
{
  scm_c_define_gsubr("forty-two", 0, 0, 0, forty_two);
  scm_c_define_gsubr("from-array-empty", 0, 0, 0, from_array_empty);
  scm_c_define_gsubr("from-array-three-elements", 0, 0, 0, from_array_three_elements);
  scm_c_define_gsubr("from-array-stop-at-zero", 0, 0, 0, from_array_stop_at_zero);
  scm_c_define_gsubr("from-array-at-least-one", 0, 0, 0, from_array_at_least_one);
  scm_c_define_gsubr("first-offset-is-zero", 0, 0, 0, first_offset_is_zero);
  scm_c_define_gsubr("second-offset-correct", 0, 0, 0, second_offset_correct);
  scm_c_define_gsubr("zero-offset-for-null-pointer", 0, 0, 0, zero_offset_for_null_pointer);
  scm_c_define_gsubr("pack-byte-audio-sample", 0, 0, 0, pack_byte_audio_sample);
  scm_c_define_gsubr("pack-byte-audio-samples", 0, 0, 0, pack_byte_audio_samples);
  scm_c_define_gsubr("pack-short-int-audio-samples", 0, 0, 0, pack_short_int_audio_samples);
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
}
