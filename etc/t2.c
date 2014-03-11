#include <stdlib.h>

void test(int *r, int *x, int n);

int main(int argc, long *argv[])
{
  int n = 3;
  int *x = (int *)malloc(n * sizeof(int));
  int *r = (int *)malloc(n * sizeof(int));
  test(r, x, n);
  free(x);
  free(r);
  return 0;
}
