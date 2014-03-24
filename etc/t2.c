#include <stdlib.h>

void test(int *r, char *x, int n);

int main(int argc, long *argv[])
{
  int n = 3;
  char *x = (char *)malloc(n * sizeof(char));
  int *r = (int *)malloc(n * sizeof(int));
  test(r, x, n);
  free(x);
  free(r);
  return 0;
}
