#include <stdlib.h>

void test(short *r, short *x, short n);

int main(int argc, long *argv[])
{
  int n = 3;
  short *x = (short *)malloc(n * sizeof(short));
  short *r = (short *)malloc(n * sizeof(short));
  test(r, x, n);
  free(x);
  free(r);
  return 0;
}
