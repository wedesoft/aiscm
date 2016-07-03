#ifndef __HELPERS_H
#define __HELPERS_H

#include <libguile.h>


SCM from_non_zero_array(int source[], int upto, int atleast);

void offsets_from_pointers(uint8_t *pointers[], int offsets[], int n);

#endif
