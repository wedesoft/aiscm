#include <libguile.h>

SCM scm_negate(SCM x)
{
  return scm_difference(x, SCM_UNDEFINED);
}

SCM scm_shr(SCM x, SCM y)
{
  return scm_ash(x, scm_negate(y));
}

void init_obj(void)
{
}
