#include <libguile.h>

SCM obj_negate(SCM x)
{
  return scm_difference(x, SCM_UNDEFINED);
}

unsigned char obj_zero_p(SCM x)
{
  return scm_is_true(scm_zero_p(x));
}

unsigned char obj_nonzero_p(SCM x)
{
  return scm_is_false(scm_zero_p(x));
}

unsigned char obj_not(SCM x)
{
  return scm_is_true(scm_not(x));
}

SCM obj_and(SCM a, SCM b)
{
  return scm_is_true(a) ? b : a;
}

SCM obj_or(SCM a, SCM b)
{
  return scm_is_true(a) ? a : b;
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

SCM obj_from_bool(unsigned char x)
{
  return scm_from_bool(x);
}

void init_obj(void)
{
}
