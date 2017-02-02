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
