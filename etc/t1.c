#include <stdlib.h>

int f(int a, int b, int c, int d, int e, int f, int g)
{
  return g;
}

int h(int a)
{
  return f(a, a, a, a, a, a, a);
}
