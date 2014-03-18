void test(int *r, int *x, int n)
{
  int *p = x;
  int *q = r;
  int *qend = r + n;
  while (q != qend) {
    int x = *p;
    *q++ = x == 0;
    *q++ = *p == *q;
    *q++ = *p == 2;
    *q++ = x == 0;
    p++;
  }
}
