// gcc -g -c t1.c
// objdump -d -M intel -S t1.o 
//
// gcc -fverbose-asm -S t1.c
// gcc -O -fverbose-asm -S t1.c
int fun(int a, int b) { return a + b; }

int main(void)
{
  int c = fun(123, 456);
  return c;
}
