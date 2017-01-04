// AIscm - Guile extension for numerical arrays and tensors.
// Copyright (C) 2013, 2014, 2015, 2016 Jan Wedekind <jan@wedesoft.de>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
#include "ffmpeg-helpers.h"


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
  int c;
  for (c=0; c<channels; c++) {
    uint8_t *p = destination + c * data_size;
    uint8_t *q = pointers[c];
    uint8_t *qend = q + nb_samples * data_size;
    int offset = (channels - 1) * data_size;
    while (q != qend) {
      int b;
      for (b=0; b<data_size; b++)
        *p++ = *q++;
      p += offset;
    };
  };
}
