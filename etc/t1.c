int test(char *p, short *q, int *r, long *s)
{
  char a = *p;
  short b = *q;
  int c = *r;
  long d = *s;
  *p = a + 1;
  *q = b + 1;
  *r = c + 1;
  *s = d + 1;
  return a + b + c + d;
}
