// AIscm - Guile extension for numerical arrays and tensors.
// Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Wedekind <jan@wedesoft.de>
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
#include <libguile.h>
#include "util-helpers.h"


SCM gc_malloc(SCM scm_size)
{
  size_t size = scm_to_int(scm_size);
  void *ptr = scm_gc_malloc(size, "scm_gc_malloc");
  SCM *p = ptr;
  int i;
  for (i=0; i<size; i+=8)
    *p++ = SCM_BOOL_F;
  return scm_from_pointer(ptr, NULL);
}

SCM gc_malloc_pointerless(SCM scm_size)
{
  size_t size = scm_to_int(scm_size);
  void *ptr = scm_gc_malloc_pointerless(size, "scm_gc_malloc_pointerless");
  return scm_from_pointer(ptr, NULL);
}

void init_util(void)
{
  scm_c_define_gsubr("gc-malloc"            , 1, 0, 0, SCM_FUNC(gc_malloc            ));
  scm_c_define_gsubr("gc-malloc-pointerless", 1, 0, 0, SCM_FUNC(gc_malloc_pointerless));
}
