#include <stdlib.h>

char f(char **p, char *q)
{
  **p = *q;
}
