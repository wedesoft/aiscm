// gcc -g -c t1.c
// objdump -d -M intel -S t1.o 
//
// gcc -fverbose-asm -S t1.c
// gcc -O -fverbose-asm -S t1.c

#include <stdio.h>

int fun(int a, int b) { return a + b; }

int main(int argc, char *argv[])
{
  int a = atoi(argv[1]);
  int b = atoi(argv[2]);
  int c = fun(a, b);
  return c;
}
