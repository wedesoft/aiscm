void test(int *r, int *x, int n)
{
  int *p = x;
  int *q = r;
  int *qend = r + n;
  while (q != qend) {
    *q++ = -*p++;
  };
}
