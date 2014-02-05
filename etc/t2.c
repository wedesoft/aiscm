#include <stdio.h>

int fun(int a, int b, int c, int d, int e, int f, int g, int h);

int main(int argc, char *argv[])
{
  int a = atoi(argv[1]);
  int b = atoi(argv[2]);
  int c = atoi(argv[3]);
  int d = atoi(argv[4]);
  int e = atoi(argv[5]);
  int f = atoi(argv[6]);
  int g = atoi(argv[5]);
  int h = atoi(argv[6]);
  int i = fun(a, b, c, d, e, f, g, h);
  printf("%d\n", i);
  return 0;
}
