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
#include "samples-helpers.h"
#include "image-helpers.h"


static void samples_setup(SCM scm_type, enum AVSampleFormat *format, int *rate, int64_t *layout, int *samples,
                          int64_t offsets[], uint8_t *data[], void *ptr)
{
  *format = scm_to_int(scm_car(scm_type));
  *rate = scm_to_int(scm_caddr(scm_type));
  *layout = av_get_default_channel_layout(scm_to_int(scm_caadr(scm_type)));
  *samples = scm_to_int(scm_cadadr(scm_type));
  scm_to_long_array(scm_cadddr(scm_type), offsets);
  pointers_from_offsets(ptr, offsets, data, AV_NUM_DATA_POINTERS);
}

SCM samples_convert(SCM scm_source_ptr, SCM scm_source_type, SCM scm_dest_ptr, SCM scm_dest_type)
{
  enum AVSampleFormat source_format;
  int source_rate;
  int64_t source_layout;
  int source_samples;
  int64_t source_offsets[AV_NUM_DATA_POINTERS];
  uint8_t *source_data[AV_NUM_DATA_POINTERS];
  void *source_ptr = scm_to_pointer(scm_source_ptr);
  samples_setup(scm_source_type, &source_format, &source_rate, &source_layout, &source_samples,
                source_offsets, source_data, source_ptr);

  enum AVSampleFormat dest_format;
  int dest_rate;
  int64_t dest_layout;
  int dest_samples;
  int64_t dest_offsets[AV_NUM_DATA_POINTERS];
  uint8_t *dest_data[AV_NUM_DATA_POINTERS];
  void *dest_ptr = scm_to_pointer(scm_dest_ptr);
  samples_setup(scm_dest_type, &dest_format, &dest_rate, &dest_layout, &dest_samples,
                dest_offsets, dest_data, dest_ptr);

  SwrContext *swr_ctx =
    swr_alloc_set_opts(NULL, dest_layout, dest_format, dest_rate, source_layout, source_format, source_rate, 0, NULL);
  if (!swr_ctx)
    scm_misc_error("samples-convert", "Could not allocate resampler context", SCM_EOL);
  int err = swr_init(swr_ctx);
  if (err < 0) {
    swr_free(&swr_ctx);
    scm_misc_error("samples-convert", "Could not initialize resampler context", SCM_EOL);
  };

  // Note: delay (swr_get_delay) not supported, i.e. converting to a different sampling rate is not supported.
  err = swr_convert(swr_ctx, dest_data, dest_samples, (const uint8_t **)source_data, source_samples);
  if (err < 0) {
    swr_free(&swr_ctx);
    scm_misc_error("samples-convert", "Error converting samples", SCM_EOL);
  };

  swr_free(&swr_ctx);
  return SCM_UNSPECIFIED;
}

void init_samples(void)
{
  scm_c_define_gsubr("samples-convert", 4, 0, 0, samples_convert);
  scm_c_define("AV_SAMPLE_FMT_U8"  ,scm_from_int(AV_SAMPLE_FMT_U8  ));
  scm_c_define("AV_SAMPLE_FMT_S16" ,scm_from_int(AV_SAMPLE_FMT_S16 ));
  scm_c_define("AV_SAMPLE_FMT_S32" ,scm_from_int(AV_SAMPLE_FMT_S32 ));
  scm_c_define("AV_SAMPLE_FMT_FLT" ,scm_from_int(AV_SAMPLE_FMT_FLT ));
  scm_c_define("AV_SAMPLE_FMT_DBL" ,scm_from_int(AV_SAMPLE_FMT_DBL ));
  scm_c_define("AV_SAMPLE_FMT_U8P" ,scm_from_int(AV_SAMPLE_FMT_U8P ));
  scm_c_define("AV_SAMPLE_FMT_S16P",scm_from_int(AV_SAMPLE_FMT_S16P));
  scm_c_define("AV_SAMPLE_FMT_S32P",scm_from_int(AV_SAMPLE_FMT_S32P));
  scm_c_define("AV_SAMPLE_FMT_FLTP",scm_from_int(AV_SAMPLE_FMT_FLTP));
  scm_c_define("AV_SAMPLE_FMT_DBLP",scm_from_int(AV_SAMPLE_FMT_DBLP));
}
