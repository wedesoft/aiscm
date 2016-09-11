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

int jit_constant_fun(void)
{
  return 42;
}

int jit_subtracting_fun(int a, int b)
{
  return a - b;
}

int jit_seven_arguments(int a, int b, int c, int d, int e, int f, int g)
{
  return g;
}

void init_jit_tests(void)
{
  scm_c_define_gsubr("jit-reset-side-effect", 0, 0, 0, jit_reset_side_effect);
}
