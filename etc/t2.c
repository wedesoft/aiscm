#include <stdio.h>

long fun(long a, long b, long c, long d, long e, long f, long g, long h);

int main(int argc, long *argv[])
{
  long a = atoi(argv[1]);
  long b = atoi(argv[2]);
  long c = atoi(argv[3]);
  long d = atoi(argv[4]);
  long e = atoi(argv[5]);
  long f = atoi(argv[6]);
  long g = atoi(argv[5]);
  long h = atoi(argv[6]);
  long i = fun(a, b, c, d, e, f, g, h);
  printf("%d\n", i);
  return 0;
}
