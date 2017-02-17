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
#include <libavutil/channel_layout.h>
#include <libswresample/swresample.h>


SCM samples_convert(SCM scm_ptr, SCM scm_source_type, SCM scm_dest_ptr, SCM scm_dest_type)
{
  SwrContext *swr_ctx =
    swr_alloc_set_opts(NULL, AV_CH_LAYOUT_STEREO, AV_SAMPLE_FMT_S32, 44100, AV_CH_LAYOUT_STEREO, AV_SAMPLE_FMT_S16, 44100, 0, NULL);
  if (!swr_ctx)
    scm_misc_error("samples-convert", "Could not allocate resampler context", SCM_EOL);
  swr_free(&swr_ctx);
  return SCM_UNDEFINED;
}

void init_samples(void)
{
  scm_c_define_gsubr("samples-convert", 4, 0, 0, samples_convert);
}
