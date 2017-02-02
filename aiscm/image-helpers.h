#ifndef __AISCM_IMAGE_HELPERS_H
#define __AISCM_IMAGE_HELPERS_H

#include <libguile.h>

void scm_to_int_array(SCM source, int32_t dest[]);

void scm_to_long_array(SCM source, int64_t dest[]);

#endif

