#include <stdlib.h>

typedef struct {
  int a;
  int b;
  char c;
  float d;
} test_t;

int f(test_t t)
{
  return t.a + t.b + t.c + (int)t.d;
}

test_t g(int a, int b, char c, float d)
{
  test_t result = {a, b, c, d};
  return result;
}
