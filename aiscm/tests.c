#include <libguile.h>

SCM forty_two(void)
{
  return scm_from_int(42);
}

void init_tests(void)
{
  scm_c_define_gsubr("forty-two", 0, 0, 0, forty_two);
}
