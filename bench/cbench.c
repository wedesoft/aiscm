#include <libguile.h>

SCM allocation(SCM scm_size)
{
  int n = scm_to_int(scm_size);
  int *r = malloc(n * sizeof(int));
  free(r);
  return SCM_UNDEFINED;
}

void init_bench(void)
{
  scm_c_define_gsubr("allocation", 1, 0, 0, allocation);
}
