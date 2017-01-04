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
#include <libguile.h>


void init_ringbuffer_tests(void);
void init_ffmpeg_helpers_tests(void);
void init_jit_tests(void);

void init_tests(void)
{
  init_ringbuffer_tests();
  init_ffmpeg_helpers_tests();
  init_jit_tests();
}
