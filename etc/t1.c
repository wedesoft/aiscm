void test(int *r, char *x, int n)
{
  char *p = x;
  int *q = r;
  int *qend = r + n;
  while (q != qend) {
    char  *x1 = (char  *)q;
    short *x2 = (short *)q;
    int   *x3 = (int   *)q;
    int c = *p;
    *x1 = (char)c;
    *x2 = (short)c;
    *x3 = c;
    p++;
    q++;
  };
}
