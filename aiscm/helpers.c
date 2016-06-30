#include "helpers.h"


SCM from_non_zero_array(int source[], int upto, int atleast)
{
  SCM retval;
  if (atleast > 0 || (upto > 0 && *source))
    retval = scm_cons(scm_from_int(*source), from_non_zero_array(source + 1, upto - 1, atleast - 1));
  else
    retval = SCM_EOL;
  return retval;
}
