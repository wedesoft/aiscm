void test(short *p, int n)
{
  short *q = p + n + 1;
  while (p != q)
    *p++ = 0;
}
