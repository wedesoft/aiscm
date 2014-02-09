#include <libguile.h>

SCM gc_malloc_pointerless_wrapper(SCM size)
{
  void *mem = scm_gc_malloc_pointerless(scm_to_int(size), "mem");
  SCM ptr = scm_from_pointer(mem, NULL);
  return ptr;
}

void init_mem(void)
{
  scm_c_define_gsubr("gc-malloc-pointerless", 1, 0, 0, gc_malloc_pointerless_wrapper);
}
