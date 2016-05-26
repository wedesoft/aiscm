extern char x;
extern char y;

char f(void)
{
  return x > y ? x : y;
}
