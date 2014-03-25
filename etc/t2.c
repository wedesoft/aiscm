#include <stdlib.h>

int test(char *p, short *q, int *r, long *s);

int main(int argc, long *argv[])
{
  int n = 3;
  int *x = (int *)malloc(n * sizeof(int));
  test((char *)x, (short *)x, (int *)x, (long *)x);
  free(x);
  return 0;
}
