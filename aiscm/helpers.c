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

// Pack audio samples.
void pack_audio(uint8_t *pointers[], int channels, int nb_samples, int data_size, uint8_t *destination)
{
  int i;
  for (i=0; i<nb_samples; i++) {
    int c;
    for (c=0; c<channels; c++) {
      memcpy(destination, pointers[c] + i * data_size, data_size);
      destination += data_size;
    };
  };
}
