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

SCM obj_negate(SCM x)
{
  return scm_difference(x, SCM_UNDEFINED);
}

unsigned char obj_zero_p(SCM x)
{
  return scm_is_true(scm_zero_p(x));
}

unsigned char obj_nonzero_p(SCM x)
{
  return scm_is_false(scm_zero_p(x));
}

unsigned char obj_not(SCM x)
{
  return scm_is_true(scm_not(x));
}

SCM obj_and(SCM a, SCM b)
{
  return scm_is_true(a) ? b : a;
}

SCM obj_or(SCM a, SCM b)
{
  return scm_is_true(a) ? a : b;
}

SCM obj_shr(SCM x, SCM y)
{
  return scm_ash(x, obj_negate(y));
}

char obj_equal_p(SCM x, SCM y)
{
  return scm_is_true(scm_equal_p(x, y));
}

char obj_nequal_p(SCM x, SCM y)
{
  return scm_is_false(scm_equal_p(x, y));
}

char obj_less_p(SCM x, SCM y)
{
  return scm_is_true(scm_less_p(x, y));
}

char obj_leq_p(SCM x, SCM y)
{
  return scm_is_true(scm_leq_p(x, y));
}

char obj_gr_p(SCM x, SCM y)
{
  return scm_is_true(scm_gr_p(x, y));
}

char obj_geq_p(SCM x, SCM y)
{
  return scm_is_true(scm_geq_p(x, y));
}

SCM obj_where(SCM m, SCM a, SCM b)
{
  return scm_is_true(m) ? a : b;
}

SCM obj_from_bool(unsigned char x)
{
  return scm_from_bool(x);
}

void init_obj(void)
{
}
