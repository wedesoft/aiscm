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


SCM samples_convert(SCM scm_source_ptr, SCM scm_source_type, SCM scm_dest_ptr, SCM scm_dest_type)
{
  SwrContext *swr_ctx =
    swr_alloc_set_opts(NULL, AV_CH_LAYOUT_STEREO, AV_SAMPLE_FMT_S32, 44100, AV_CH_LAYOUT_STEREO, AV_SAMPLE_FMT_S16, 44100, 0, NULL);
  if (!swr_ctx)
    scm_misc_error("samples-convert", "Could not allocate resampler context", SCM_EOL);
  int err = swr_init(swr_ctx);
  if (err < 0)
    scm_misc_error("samples-convert", "Could not initialize resampler context", SCM_EOL);

  uint8_t *source_ptr = scm_to_pointer(scm_source_ptr);
  int source_samples = scm_to_int(scm_cadadr(scm_source_type));

  uint8_t *dest_ptr = scm_to_pointer(scm_dest_ptr);
  int dest_samples = scm_to_int(scm_cadadr(scm_dest_type));

  swr_convert(swr_ctx, &dest_ptr, dest_samples, (const uint8_t **)&source_ptr, source_samples);

  swr_free(&swr_ctx);
  return SCM_UNDEFINED;
}

void init_samples(void)
{
  scm_c_define_gsubr("samples-convert", 4, 0, 0, samples_convert);
}
