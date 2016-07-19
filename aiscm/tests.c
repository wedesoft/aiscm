#include <libguile.h>
#include "ffmpeg-helpers.h"


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

SCM ring_buffer_empty_initially(void)
{
  struct ring_buffer_t ring_buffer;
  ring_buffer_init(&ring_buffer, 10);
  SCM retval = scm_from_bool(ring_buffer.count == 0);
  ring_buffer_free(&ring_buffer);
  return retval;
}

SCM ring_buffer_adding_element(void)
{
  int element;
  struct ring_buffer_t ring_buffer;
  ring_buffer_init(&ring_buffer, 2);
  ring_buffer_push(&ring_buffer, &element);
  SCM retval = scm_from_bool(ring_buffer.count == 1);
  ring_buffer_free(&ring_buffer);
  return retval;
}

SCM ring_buffer_retrieve_element(void)
{
  int element;
  struct ring_buffer_t ring_buffer;
  ring_buffer_init(&ring_buffer, 2);
  ring_buffer_push(&ring_buffer, &element);
  SCM retval = scm_from_bool(&element == ring_buffer_pop(&ring_buffer));
  ring_buffer_free(&ring_buffer);
  return retval;
}

SCM ring_buffer_element_removed(void)
{
  int element;
  struct ring_buffer_t ring_buffer;
  ring_buffer_init(&ring_buffer, 2);
  ring_buffer_push(&ring_buffer, &element);
  ring_buffer_pop(&ring_buffer);
  SCM retval = scm_from_bool(ring_buffer.count == 0);
  ring_buffer_free(&ring_buffer);
  return retval;
}

SCM ring_buffer_is_fifo(void)
{
  int one, two;
  struct ring_buffer_t ring_buffer;
  ring_buffer_init(&ring_buffer, 2);
  ring_buffer_push(&ring_buffer, &one);
  ring_buffer_push(&ring_buffer, &two);
  int *a = ring_buffer_pop(&ring_buffer);
  int *b = ring_buffer_pop(&ring_buffer);
  SCM retval = scm_from_bool(a == &one && b == &two);
  ring_buffer_free(&ring_buffer);
  return retval;
}

SCM ring_buffer_interleaved(void)
{
  int one, two, three;
  struct ring_buffer_t ring_buffer;
  ring_buffer_init(&ring_buffer, 3);
  ring_buffer_push(&ring_buffer, &one);
  ring_buffer_push(&ring_buffer, &two);
  int *a = ring_buffer_pop(&ring_buffer);
  ring_buffer_push(&ring_buffer, &three);
  int *b = ring_buffer_pop(&ring_buffer);
  int *c = ring_buffer_pop(&ring_buffer);
  SCM retval = scm_from_bool(a == &one && b == &two && c == &three);
  ring_buffer_free(&ring_buffer);
  return retval;
}

SCM ring_buffer_cyclical(void)
{
  int one, two, three;
  struct ring_buffer_t ring_buffer;
  ring_buffer_init(&ring_buffer, 2);
  ring_buffer_push(&ring_buffer, &one);
  ring_buffer_push(&ring_buffer, &two);
  int *a = ring_buffer_pop(&ring_buffer);
  ring_buffer_push(&ring_buffer, &three);
  int *b = ring_buffer_pop(&ring_buffer);
  int *c = ring_buffer_pop(&ring_buffer);
  SCM retval = scm_from_bool(a == &one && b == &two && c == &three && ring_buffer.offset < 2);
  ring_buffer_free(&ring_buffer);
  return retval;
}

SCM ring_buffer_test_full(void)
{
  int one;
  struct ring_buffer_t ring_buffer;
  ring_buffer_init(&ring_buffer, 1);
  ring_buffer_push(&ring_buffer, &one);
  SCM retval = scm_from_bool(ring_buffer_full(&ring_buffer) && !ring_buffer_empty(&ring_buffer));
  ring_buffer_free(&ring_buffer);
  return retval;
}

SCM ring_buffer_test_empty(void)
{
  struct ring_buffer_t ring_buffer;
  ring_buffer_init(&ring_buffer, 1);
  SCM retval = scm_from_bool(!ring_buffer_full(&ring_buffer) && ring_buffer_empty(&ring_buffer));
  ring_buffer_free(&ring_buffer);
  return retval;
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
  scm_c_define_gsubr("ring-buffer-empty-initially", 0, 0, 0, ring_buffer_empty_initially);
  scm_c_define_gsubr("ring-buffer-adding-element", 0, 0, 0, ring_buffer_adding_element);
  scm_c_define_gsubr("ring-buffer-retrieve-element", 0, 0, 0, ring_buffer_retrieve_element);
  scm_c_define_gsubr("ring-buffer-element-removed", 0, 0, 0, ring_buffer_element_removed);
  scm_c_define_gsubr("ring-buffer-is-fifo", 0, 0, 0, ring_buffer_is_fifo);
  scm_c_define_gsubr("ring-buffer-interleaved", 0, 0, 0, ring_buffer_interleaved);
  scm_c_define_gsubr("ring-buffer-cyclical", 0, 0, 0, ring_buffer_cyclical);
  scm_c_define_gsubr("ring-buffer-test-full", 0, 0, 0, ring_buffer_test_full);
  scm_c_define_gsubr("ring-buffer-test-empty", 0, 0, 0, ring_buffer_test_empty);
}
