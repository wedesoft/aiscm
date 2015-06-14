#include <libguile.h>

SCM gc_malloc_pointerless(SCM scm_size)
{
  size_t size = scm_to_int(scm_size);
  void *ptr = scm_gc_malloc_pointerless(size, "aiscm pointer");
  return scm_from_pointer(ptr, NULL);
}

void init_util(void)
{
  scm_c_define_gsubr("gc-malloc-pointerless", 1, 0, 0, gc_malloc_pointerless);
}
