#include <libguile.h>
#include "helpers.h"


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
  uint16_t *data[2] = {a, b};
  uint16_t destination[6];
  pack_audio(data, 2, 3, sizeof(uint16_t), destination);
  return scm_from_bool(destination[4] == 5 && destination[5] == 6);
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
}
