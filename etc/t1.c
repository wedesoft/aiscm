int f(int a, int b, int c, int d, int e, int f, int g, int h)
{
  return a + b + c + d + e + f + g + h;
}

int g(int a, int b, int c, int d, int e, int f, int g, int h);

int h(int x, int y)
{
  return g(x, 1, 2, 3, 4, 5, y, 7);
}
