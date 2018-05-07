// AIscm - Guile extension for numerical arrays and tensors.
// Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 Jan Wedekind <jan@wedesoft.de>
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
#include "util-helpers.h"


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
  return scm_from_bool(dest[0] == (123L << 32));
}

SCM scm_to_long_array_second_element(SCM source)
{
  long dest[2];
  scm_to_long_array(source, dest);
  return scm_from_bool(dest[1] == (456L << 32));
}

static scm_t_bits test_type_tag;

struct test_type_t {
  char *ptr;
};

SCM cleanup_test_type(SCM scm_self)
{
  struct test_type_t *self = (struct test_type_t *)SCM_SMOB_DATA(scm_self);
  *self->ptr = 1;
  return SCM_UNSPECIFIED;
}

SCM call_scheme_function(SCM scm_fun, SCM scm_arg)
{
  return clean_up_on_failure(SCM_UNDEFINED, NULL, scm_fun, scm_arg);
}

static SCM call_clean_up_on_failure(void *data)
{
  SCM *body_data = (SCM *)data;
  SCM test_object = body_data[0];
  SCM scm_fun = body_data[1];
  return clean_up_on_failure(test_object, cleanup_test_type, scm_fun, SCM_UNDEFINED);
}

static SCM ignore_exception(void *data, SCM tag, SCM throw_args)
{
  return SCM_BOOL_F;
}

SCM cleanup_when_exception(SCM scm_fun)
{
  char cleaned = 0;
  SCM test_object;
  struct test_type_t *self = (struct test_type_t *)scm_gc_calloc(sizeof(struct test_type_t), "testing");
  SCM_NEWSMOB(test_object, test_type_tag, self);
  self->ptr = &cleaned;
  SCM body_data[2];
  body_data[0] = test_object;
  body_data[1] = scm_fun;
  scm_c_catch(SCM_BOOL_T, call_clean_up_on_failure, &body_data, ignore_exception, NULL, NULL, NULL);
  char retval = cleaned;
  scm_remember_upto_here_1(test_object);
  return scm_from_bool(retval);
}

SCM throw_exception_after_cleanup(SCM scm_fun)
{
  char cleaned;
  SCM test_object;
  struct test_type_t *self = (struct test_type_t *)scm_gc_calloc(sizeof(struct test_type_t), "testing");
  SCM_NEWSMOB(test_object, test_type_tag, self);
  self->ptr = &cleaned;
  clean_up_on_failure(test_object, cleanup_test_type, scm_fun, SCM_UNDEFINED);
  return SCM_UNDEFINED;
}

void init_util_helpers_tests(void)
{
  test_type_tag = scm_make_smob_type("test-type", sizeof(struct test_type_t));
  scm_c_define_gsubr("scm-to-int-array-one-element"    , 1, 0, 0, SCM_FUNC(scm_to_int_array_one_element    ));
  scm_c_define_gsubr("scm-to-int-array-second-element" , 1, 0, 0, SCM_FUNC(scm_to_int_array_second_element ));
  scm_c_define_gsubr("scm-to-long-array-one-element"   , 1, 0, 0, SCM_FUNC(scm_to_long_array_one_element   ));
  scm_c_define_gsubr("scm-to-long-array-second-element", 1, 0, 0, SCM_FUNC(scm_to_long_array_second_element));
  scm_c_define_gsubr("call-scheme-function"            , 2, 0, 0, SCM_FUNC(call_scheme_function            ));
  scm_c_define_gsubr("cleanup-when-exception"          , 1, 0, 0, SCM_FUNC(cleanup_when_exception          ));
  scm_c_define_gsubr("throw-exception-after-cleanup"   , 1, 0, 0, SCM_FUNC(throw_exception_after_cleanup   ));
}
