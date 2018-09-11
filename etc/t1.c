#include <stdlib.h>

typedef struct {
  long a;
  long b;
  long c;
} test_t;

test_t f();

long g()
{
  test_t x = f();
  return x.a + x.b + x.c;
}
