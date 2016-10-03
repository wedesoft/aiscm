#include <libguile.h>

SCM obj_negate(SCM x)
{
  return scm_difference(x, SCM_UNDEFINED);
}

char obj_zero_p(SCM x)
{
  return scm_is_true(scm_zero_p(x));
}

char obj_nonzero_p(SCM x)
{
  return scm_is_false(scm_zero_p(x));
}

SCM obj_shr(SCM x, SCM y)
{
  return scm_ash(x, obj_negate(y));
}

char obj_equal_p(SCM x, SCM y)
{
  return scm_is_true(scm_equal_p(x, y));
}

char obj_nequal_p(SCM x, SCM y)
{
  return scm_is_false(scm_equal_p(x, y));
}

char obj_less_p(SCM x, SCM y)
{
  return scm_is_true(scm_less_p(x, y));
}

char obj_leq_p(SCM x, SCM y)
{
  return scm_is_true(scm_leq_p(x, y));
}

char obj_gr_p(SCM x, SCM y)
{
  return scm_is_true(scm_gr_p(x, y));
}

char obj_geq_p(SCM x, SCM y)
{
  return scm_is_true(scm_geq_p(x, y));
}

void init_obj(void)
{
}
