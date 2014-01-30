#include <libguile.h>
#include <sys/mman.h>

// http://blog.reverberate.org/2012/12/hello-jit-world-joy-of-simple-jits.html

static scm_t_bits mmap_tag;

struct mmap_t {
  void *mem;
  int len;
};

size_t free_mmap(SCM mmap_smob)
{
  struct mmap_t *mem = (struct mmap_t *)SCM_SMOB_DATA(mmap_smob);
  munmap(mem->mem, mem->len);
  scm_gc_free(mem, sizeof(struct mmap_t), "mmap");
  return 0;
}

SCM make_mmap(SCM code)
{
  SCM retval;
  struct mmap_t *mem;
  mem = (struct mmap_t *)scm_gc_malloc(sizeof(struct mmap_t), "mmap");
  mem->len = SCM_BYTEVECTOR_LENGTH(code);
  mem->mem = mmap(NULL, mem->len, PROT_WRITE | PROT_EXEC,
                  MAP_ANON | MAP_PRIVATE, -1, 0);
  SCM_NEWSMOB(retval, mmap_tag, mem);
  memcpy(mem->mem, SCM_BYTEVECTOR_CONTENTS(code), SCM_BYTEVECTOR_LENGTH(code));
  scm_gc_register_allocation(mem->len);
  return retval;
}

SCM mmap_call(SCM mmap_smob)
{
  scm_assert_smob_type(mmap_tag, mmap_smob);
  struct mmap_t *mem = (struct mmap_t *)SCM_SMOB_DATA(mmap_smob);
  int (*func)() = mem->mem;
  int result = func();
  scm_remember_upto_here_1(mmap_smob);
  return scm_from_int(result);
}

void init_jit(void)
{
  mmap_tag = scm_make_smob_type("mmap", sizeof(void *));
  scm_set_smob_free(mmap_tag, free_mmap);
  scm_c_define_gsubr("mmap-call", 1, 0, 0, mmap_call);
  scm_c_define_gsubr("make-mmap", 1, 0, 0, make_mmap);
}
