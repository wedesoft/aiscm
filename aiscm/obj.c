#include <libguile.h>

SCM scm_negate(SCM x)
{
  return scm_difference(x, SCM_UNDEFINED);
}

SCM scm_shr(SCM x, SCM y)
{
  return scm_ash(x, scm_negate(y));
}

char obj_equal(SCM x, SCM y)
{
  return scm_is_true(scm_equal_p(x, y));
}

char obj_nequal(SCM x, SCM y)
{
  return scm_is_false(scm_equal_p(x, y));
}

void init_obj(void)
{
}
