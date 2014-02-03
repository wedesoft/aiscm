#include <stdio.h>

int main(int argc, char *argv[])
{ 
  long x = atol(argv[1]);
  printf("%d\n", x);
  printf("%d\n", x >> 7);
  return 0;
}
