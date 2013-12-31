#include <libguile.h>

SCM make_malloc_wrapper(SCM size)
{
  void *mem = scm_gc_malloc_pointerless(scm_to_int(size), "aiscm");
  SCM ptr = scm_from_pointer(mem, NULL);
  return scm_cons(ptr, scm_cons(ptr, scm_cons(size, SCM_EOL)));
}

SCM malloc_plus_wrapper(SCM malloc, SCM offset)
{
  void *mem = SCM_POINTER_VALUE(scm_car(malloc));
  int size = scm_to_int(scm_caddr(malloc));
  int diff = scm_to_int(offset);
  if (diff < 0)
    scm_throw(scm_str2symbol("malloc-plus-offset-lt-zero"),
              scm_list_1(offset));
  if (diff > size)
    scm_throw(scm_str2symbol("malloc-plus-offset-gt-size"),
              scm_list_2(offset, scm_caddr(malloc)));
  SCM ptr = scm_from_pointer(mem + diff, NULL);
  return scm_cons(ptr,
                  scm_cons(scm_cadr(malloc),
                           scm_cons(scm_from_int(size - diff),
                                    SCM_EOL)));
}

void init_malloc(void)
{
  scm_c_define_gsubr("make-malloc", 1, 0, 0, make_malloc_wrapper);
  scm_c_define_gsubr("malloc-plus", 2, 0, 0, malloc_plus_wrapper);
}
