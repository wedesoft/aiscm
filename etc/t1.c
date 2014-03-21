void test(short *r, short *x, short n)
{
  short *p = x;
  short *q = r;
  short *qend = r + n;
  while (q != qend) {
    short x = *p;
    *q++ = x == 0;
    *q++ = *p == *q;
    *q++ = *p == 2;
    *q++ = x == 0;
    p++;
  }
}
