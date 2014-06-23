void other(int a);

int test(int *p)
{
  int a = *p;
  other(a);
  return a;
}
