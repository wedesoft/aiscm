#include <libguile.h>

static int side_effect;

SCM jit_reset_side_effect(void)
{
  int retval = side_effect;
  side_effect = 0;
  return scm_from_int(retval);
}

void jit_side_effect(void)
{
  side_effect = 42;
}

void init_jit_tests(void)
{
  scm_c_define_gsubr("jit-reset-side-effect", 0, 0, 0, jit_reset_side_effect);
}
