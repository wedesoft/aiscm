// AIscm - Guile extension for numerical arrays and tensors.
// Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 Jan Wedekind <jan@wedesoft.de>
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


static int side_effect;

SCM jit_reset_side_effect(void)
{
  int retval = side_effect;
  side_effect = 0;
  return scm_from_int(retval);
}

void jit_side_effect(void)
{
  side_effect = 42;
}

int jit_constant_fun(void)
{
  return 42;
}

int jit_subtracting_fun(int a, int b)
{
  return a - b;
}

int jit_seven_arguments(int a, int b, int c, int d, int e, int f, int g)
{
  return g;
}

char jit_boolean_not(char x)
{
  return !x;
}

void init_jit_tests(void)
{
  scm_c_define_gsubr("jit-reset-side-effect", 0, 0, 0, SCM_FUNC(jit_reset_side_effect));
}
