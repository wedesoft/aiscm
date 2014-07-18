void other(int a);

int test(int *p, unsigned int s, unsigned int n)
{
  int *pend = p + n * s;
  while (p != pend) {
    other(*p);
    p += s;
  };
  return 0;
}
