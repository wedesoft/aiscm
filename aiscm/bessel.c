#include <math.h>
#include <libguile.h>

SCM j0_wrapper(SCM x)
{
  return scm_from_double(j0(scm_to_double(x)));
}

SCM malloc_wrapper(SCM x)
{
  void *mem = scm_gc_malloc_pointerless(scm_to_int(x), "aiscm");
  return scm_from_pointer(mem, NULL);
}

void init_bessel(void)
{
  scm_c_define_gsubr("j0", 1, 0, 0, j0_wrapper);
  scm_c_define_gsubr("malloc", 1, 0, 0, malloc_wrapper);
}
