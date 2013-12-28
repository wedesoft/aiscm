#include <libguile.h>

SCM malloc_wrapper(SCM size)
{
  void *mem = scm_gc_malloc_pointerless(scm_to_int(size), "aiscm");
  SCM ptr = scm_from_pointer(mem, NULL);
  return scm_cons(ptr, scm_cons(ptr, scm_cons(size, SCM_EOL)));
}

void init_aiscm(void)
{
  scm_c_define_gsubr("malloc", 1, 0, 0, malloc_wrapper);
}
