#include <libguile.h>
#include <sys/mman.h>

// http://blog.reverberate.org/2012/12/hello-jit-world-joy-of-simple-jits.html

SCM jit_call_bv(SCM code)
{
  int len = SCM_BYTEVECTOR_LENGTH(code);
  void *mem = mmap(NULL, len, PROT_WRITE | PROT_EXEC,
                   MAP_ANON | MAP_PRIVATE, -1, 0);
  memcpy(mem, SCM_BYTEVECTOR_CONTENTS(code), SCM_BYTEVECTOR_LENGTH(code));
  int (*func)() = mem;
  int result = func();
  munmap(mem, len);
  return scm_from_int(result);
}

void init_jit(void)
{
  scm_c_define_gsubr("jit-call-bv", 1, 0, 0, jit_call_bv);
}

