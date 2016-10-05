#include <libguile.h>

SCM gc_malloc(SCM scm_size)
{
  size_t size = scm_to_int(scm_size);
  void *ptr = scm_gc_malloc(size, "scm_gc_malloc");
  SCM *p = ptr;
  int i;
  for (i=0; i<size; i+=8)
    *p++ = SCM_BOOL_F;
  return scm_from_pointer(ptr, NULL);
}

SCM gc_malloc_pointerless(SCM scm_size)
{
  size_t size = scm_to_int(scm_size);
  void *ptr = scm_gc_malloc_pointerless(size, "scm_gc_malloc_pointerless");
  return scm_from_pointer(ptr, NULL);
}

void init_util(void)
{
  scm_c_define_gsubr("gc-malloc"            , 1, 0, 0, gc_malloc);
  scm_c_define_gsubr("gc-malloc-pointerless", 1, 0, 0, gc_malloc_pointerless);
}
