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
#ifndef __AISCM_IMAGE_HELPERS_H
#define __AISCM_IMAGE_HELPERS_H

#include <libguile.h>

void scm_to_int_array(SCM source, int32_t dest[]);

void scm_to_long_array(SCM source, int64_t dest[]);

SCM clean_up_on_failure(SCM scm_object, SCM (*cleanup_method)(SCM), SCM scm_fun, SCM scm_arg);

#endif

