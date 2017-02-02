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
#include "image-helpers.h"


SCM scm_to_int_array_one_element(SCM source)
{
  int dest[1];
  scm_to_int_array(source, dest);
  return scm_from_bool(dest[0] == 123);
}

SCM scm_to_int_array_second_element(SCM source)
{
  int dest[2];
  scm_to_int_array(source, dest);
  return scm_from_bool(dest[1] == 456);
}

SCM scm_to_long_array_one_element(SCM source)
{
  long dest[1];
  scm_to_long_array(source, dest);
  return scm_from_bool(dest[0] == (123 << 32));
}

SCM scm_to_long_array_second_element(SCM source)
{
  long dest[2];
  scm_to_long_array(source, dest);
  return scm_from_bool(dest[1] == (456 << 32));
}

void init_image_helpers_tests(void)
{
  scm_c_define_gsubr("scm-to-int-array-one-element", 1, 0, 0, scm_to_int_array_one_element);
  scm_c_define_gsubr("scm-to-int-array-second-element", 1, 0, 0, scm_to_int_array_second_element);
  scm_c_define_gsubr("scm-to-long-array-one-element", 1, 0, 0, scm_to_long_array_one_element);
  scm_c_define_gsubr("scm-to-long-array-second-element", 1, 0, 0, scm_to_long_array_second_element);
}
