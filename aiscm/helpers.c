#include "helpers.h"


// Convert array of integer offsets to Scheme array.
SCM from_non_zero_array(int source[], int upto, int atleast)
{
  SCM retval;
  if (atleast > 0 || (upto > 0 && *source))
    retval = scm_cons(scm_from_int(*source), from_non_zero_array(source + 1, upto - 1, atleast - 1));
  else
    retval = SCM_EOL;
  return retval;
}

// Get offsets from pointer array.
void offsets_from_pointers(uint8_t *pointers[], int offsets[], int n)
{
  int i;
  for (i=0; i<n; i++)
    offsets[i] = pointers[i] ? pointers[i] - pointers[0] : 0;
}
