#include <libguile.h>

SCM scm_negate(SCM x)
{
  return scm_difference(x, SCM_UNDEFINED);
}

void init_obj(void)
{
}
