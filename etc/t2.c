#include <stdio.h>

int *p;

int main(int argc, char *argv[])
{ 
  printf("%d\n", sizeof(p));
  int i = *p;
  return i;
}
