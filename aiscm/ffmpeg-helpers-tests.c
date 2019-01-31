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
#include "ffmpeg-helpers.h"
#include "util-helpers.h"


SCM int_array_to_long_one_element(void)
{
  int32_t array[1] = {42};
  int64_t result[1];
  int_array_to_long(result, array, 1);
  return scm_from_bool(result[0] == 42);
}

SCM int_array_to_long_second_element(void)
{
  int32_t array[2] = {42, 56};
  int64_t result[2];
  int_array_to_long(result, array, 2);
  return scm_from_bool(result[1] == 56);
}

SCM from_array_empty(void)
{
  return from_non_zero_array(NULL, 0, 0);
}

SCM from_array_three_elements(void)
{
  int64_t array[3] = {2, 3, 5};
  return from_non_zero_array(array, 3, 1);
}

SCM from_array_stop_at_zero(void)
{
  int64_t array[5] = {2, 3, 5, 0, 7};
  return from_non_zero_array(array, 5, 1);
}

SCM from_array_at_least_one(void)
{
  int64_t array[3] = {0, 0, 0};
  return from_non_zero_array(array, 3, 1);
}

SCM from_array_long_integers(void)
{
  int64_t array[1] = {42L << 32};
  return from_non_zero_array(array, 1, 1);
}

SCM first_offset_is_zero(void)
{
  uint8_t buffer[1024];
  uint8_t *data[1] = {buffer};
  int64_t offsets[1];
  offsets_from_pointers(data, offsets, 1);
  return scm_from_bool(offsets[0] == 0);
}

SCM second_offset_correct(void)
{
  uint8_t buffer[1024];
  uint8_t *data[2] = {buffer, buffer + 12};
  int64_t offsets[2];
  offsets_from_pointers(data, offsets, 2);
  return scm_from_bool(offsets[1] == 12);
}

SCM zero_offset_for_null_pointer(void)
{
  uint8_t buffer[1024];
  uint8_t *data[2] = {buffer, NULL};
  int64_t offsets[2];
  offsets_from_pointers(data, offsets, 2);
  return scm_from_bool(offsets[1] == 0);
}

SCM offsets_have_64_bit(SCM scm_offset)
{
  // This test might trigger a warning on Debian QA [1].
  //
  // ```
  // warning: array subscript is above array bounds [-Warray-bounds]
  // ```
  //
  // Note that the out-of-bounds pointer is merely used for testing offset computation.
  //
  // [1]: https://qa.debian.org/bls/packages/a/aiscm.html
  uint8_t buffer[1024];
  uint8_t *data[2];
  data[0] = buffer;
  data[1] = buffer + scm_to_long(scm_offset);
  int64_t offsets[2];
  offsets_from_pointers(data, offsets, 2);
  return scm_from_bool(offsets[1] != 0);
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

void init_ffmpeg_helpers_tests(void)
{
  scm_c_define_gsubr("int-array-to-long-one-element"   , 0, 0, 0, SCM_FUNC(int_array_to_long_one_element   ));
  scm_c_define_gsubr("int-array-to-long-second-element", 0, 0, 0, SCM_FUNC(int_array_to_long_second_element));
  scm_c_define_gsubr("from-array-empty"                , 0, 0, 0, SCM_FUNC(from_array_empty                ));
  scm_c_define_gsubr("from-array-three-elements"       , 0, 0, 0, SCM_FUNC(from_array_three_elements       ));
  scm_c_define_gsubr("from-array-stop-at-zero"         , 0, 0, 0, SCM_FUNC(from_array_stop_at_zero         ));
  scm_c_define_gsubr("from-array-at-least-one"         , 0, 0, 0, SCM_FUNC(from_array_at_least_one         ));
  scm_c_define_gsubr("from-array-long-integers"        , 0, 0, 0, SCM_FUNC(from_array_long_integers        ));
  scm_c_define_gsubr("first-offset-is-zero"            , 0, 0, 0, SCM_FUNC(first_offset_is_zero            ));
  scm_c_define_gsubr("second-offset-correct"           , 0, 0, 0, SCM_FUNC(second_offset_correct           ));
  scm_c_define_gsubr("zero-offset-for-null-pointer"    , 0, 0, 0, SCM_FUNC(zero_offset_for_null_pointer    ));
  scm_c_define_gsubr("offsets-have-64-bit"             , 1, 0, 0, SCM_FUNC(offsets_have_64_bit             ));
  scm_c_define_gsubr("pack-byte-audio-sample"          , 0, 0, 0, SCM_FUNC(pack_byte_audio_sample          ));
  scm_c_define_gsubr("pack-byte-audio-samples"         , 0, 0, 0, SCM_FUNC(pack_byte_audio_samples         ));
  scm_c_define_gsubr("pack-short-int-audio-samples"    , 0, 0, 0, SCM_FUNC(pack_short_int_audio_samples    ));
}
