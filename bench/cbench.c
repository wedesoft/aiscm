#include <libguile.h>

int *ret;

SCM allocation(SCM scm_size)
{
  int n = scm_to_int(scm_size);
  ret = malloc(n * sizeof(int));
  free(ret);
  return SCM_UNDEFINED;
}

SCM negate(SCM scm_ptr, SCM scm_size)
{
  int *p = (int *)scm_to_pointer(scm_ptr);
  int n = scm_to_int(scm_size);
  ret = malloc(n * sizeof(int));
  int *rend = ret + n;
  int *r;
  for (r=ret; r!=rend; r++, p++)
    *r = -*p;
  free(ret);
  return SCM_UNDEFINED;
}

void init_bench(void)
{
  scm_c_define_gsubr("allocation", 1, 0, 0, allocation);
  scm_c_define_gsubr("negate", 2, 0, 0, negate);
}
