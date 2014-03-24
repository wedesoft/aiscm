void test(short *r, short *x, int n)
{
  short *p = x;
  short *q = r;
  short *qend = r + n;
  while (q != qend)
    *q++ = -*p++;
}
