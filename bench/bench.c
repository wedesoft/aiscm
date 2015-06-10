#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>

//      user system total real
// Ruby 3.2   5.3   2.2   1.0

const int count = 100;

struct timeval t;

void start(struct timeval *t)
{
  gettimeofday(t, 0);
}

float stop(struct timeval *t)
{
  struct timeval t2;
  gettimeofday(&t2, 0);
  struct timeval diff;
  timersub(&t2, t, &diff);
  return diff.tv_sec + diff.tv_usec * 1.0e-6;
}

int *sequence_plus_scalar(int *p, int n, int s)
{
  int *r = malloc(n * sizeof(int));
  int i;
  int *q = r;
  for (i=0; i<n; i++)
    *q++ = *p++ + s;
  return r;
}

int main(void)
{
  struct timeval t;
  int i;
  const int n = 1000000;
  int *p = malloc(n * sizeof(int));
  free(sequence_plus_scalar(p, n, 42));
  start(&t);
  for (i=0; i<count; i++)
    free(sequence_plus_scalar(p, n, 42));
  printf("sequence plus scalar: %f\n", stop(&t));
  free(p);
  return 0;
}
