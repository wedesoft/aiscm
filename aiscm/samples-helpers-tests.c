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
#include "samples-helpers.h"

SCM first_pointer_from_offset(void)
{
  uint8_t buffer[4] = {2, 3, 5, 7};
  uint8_t *p = &buffer[0];
  uint8_t *data[1];
  int64_t offsets[1] = {3};
  pointers_from_offsets(p, offsets, data, 1);
  return scm_from_bool(7 == *data[0]);
}

SCM second_pointer_from_offset(void)
{
  uint8_t buffer[4] = {2, 3, 5, 7};
  uint8_t *p = &buffer[0];
  uint8_t *data[2];
  int64_t offsets[2] = {3, 2};
  pointers_from_offsets(p, offsets, data, 2);
  return scm_from_bool(5 == *data[1]);
}

void init_samples_helpers_tests(void)
{
  scm_c_define_gsubr("first-pointer-from-offset" , 0, 0, 0, first_pointer_from_offset );
  scm_c_define_gsubr("second-pointer-from-offset", 0, 0, 0, second_pointer_from_offset);
}
