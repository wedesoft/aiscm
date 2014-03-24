#include <stdlib.h>

int test(char *p, short *q, int *r);

int main(int argc, long *argv[])
{
  int n = 3;
  int *x = (int *)malloc(n * sizeof(int));
  test((char *)x, (short *)x, (int *)x);
  free(x);
  return 0;
}
