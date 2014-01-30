#include <stdio.h>

int fun(int a, int b) { return a + b; }

int main(int argc, char *argv[])
{
  int a = atoi(argv[1]);
  int b = atoi(argv[2]);
  int c = fun(a, b);
  printf("%d\n", c);
  return 0;
}
