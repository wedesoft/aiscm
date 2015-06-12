#include <libguile.h>

SCM array_plus_scalar(void)
{
  int *r = malloc(1000000 * sizeof(int));
  int *p = r;
  int i;
  for (i=0; i<1000000; i++)
    *p++ += 42;
  free(r);
  return SCM_UNDEFINED;
}

void init_bench(void)
{
  scm_c_define_gsubr("array+scalar", 0, 0, 0, array_plus_scalar);
}
