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


void scm_to_int_array(SCM source, int32_t dest[])
{
  if (!scm_is_null_and_not_nil(source)) {
    *dest = scm_to_int(scm_car(source));
    scm_to_int_array(scm_cdr(source), dest + 1);
  };
}

void scm_to_long_array(SCM source, int64_t dest[])
{
  if (!scm_is_null_and_not_nil(source)) {
    *dest = scm_to_long(scm_car(source));
    scm_to_long_array(scm_cdr(source), dest + 1);
  };
}

static SCM do_function_call(void *data)
{
  SCM *args = (SCM *)data;
  return scm_call_1(args[0], args[1]);
}

struct cleanup_t {
  SCM scm_object;
  SCM (*cleanup_method)(SCM);
};

static SCM do_clean_up(void *data, SCM tag, SCM throw_args)
{
  struct cleanup_t *args = (struct cleanup_t *)data;
  SCM scm_object = args->scm_object;
  SCM (*cleanup_method)(SCM) = args->cleanup_method;
  return (*cleanup_method)(scm_object);
}

SCM clean_up_on_failure(SCM scm_object, SCM (*cleanup_method)(SCM), SCM scm_fun, SCM scm_arg)
{
  SCM body_data[2];
  body_data[0] = scm_fun;
  body_data[1] = scm_arg;
  struct cleanup_t handler_data;
  handler_data.scm_object = scm_object;
  handler_data.cleanup_method = cleanup_method;
  return scm_c_catch(SCM_BOOL_T, do_function_call, &body_data, do_clean_up, &handler_data, NULL, NULL);
}
