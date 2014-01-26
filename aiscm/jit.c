#include <libguile.h>
#include <sys/mman.h>

SCM jit_call(SCM i)
{
  unsigned char code[] = {0xb8, 0x00, 0x00, 0x00, 0x00, 0xc3};
  int num = scm_to_int(i);
  memcpy(&code[1], &num, 4);
  void *mem = mmap(NULL, sizeof(code), PROT_WRITE | PROT_EXEC,
                   MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
  memcpy(mem, code, sizeof(code));
  int (*func)() = mem;
  int result = func();
  munmap(mem, sizeof(code));
  return scm_from_int(result);
}

void init_jit(void)
{
  scm_c_define_gsubr("jit-call", 1, 0, 0, jit_call);
}

