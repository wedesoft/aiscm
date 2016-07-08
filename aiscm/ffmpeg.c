#include <libguile.h>
#include <libavformat/avformat.h>
#include "config.h"
#include "helpers.h"

// http://dranger.com/ffmpeg/
// https://github.com/FFmpeg/FFmpeg/blob/n2.6.9/doc/examples/demuxing_decoding.c
// https://github.com/FFmpeg/FFmpeg/blob/n2.6.9/doc/examples/filtering_video.c

#ifndef HAVE_FRAME_ALLOC
#define av_frame_alloc avcodec_alloc_frame
#define av_frame_free avcodec_free_frame
#endif

static scm_t_bits format_context_tag;

struct format_context_t {
  AVFormatContext *fmt_ctx;
  int video_stream_idx;
  AVCodecContext *video_dec_ctx;
  int audio_stream_idx;
  AVCodecContext *audio_dec_ctx;
  AVPacket pkt;
  AVPacket orig_pkt;
  AVFrame *frame;
  int64_t video_pts;
};

SCM get_error_text(int err)
{
  static char buf[255];
  av_strerror(err, buf, sizeof(buf));
  return scm_from_locale_string(buf);
}

static struct format_context_t *get_self(SCM scm_self)
{
  scm_assert_smob_type(format_context_tag, scm_self);
  return (struct format_context_t *)SCM_SMOB_DATA(scm_self);
}

SCM format_context_destroy(SCM scm_self)
{
  scm_assert_smob_type(format_context_tag, scm_self);
  struct format_context_t *self = get_self(scm_self);
  if (self->frame) {
    av_frame_free(&self->frame);
    self->frame = NULL;
  };
  if (self->orig_pkt.data) {
    av_free_packet(&self->orig_pkt);
    self->orig_pkt.data = NULL;
  };
  if (self->audio_dec_ctx) {
    avcodec_close(self->audio_dec_ctx);
    self->audio_dec_ctx = NULL;
  };
  if (self->video_dec_ctx) {
    avcodec_close(self->video_dec_ctx);
    self->video_dec_ctx = NULL;
  };
  if (self->fmt_ctx) {
    avformat_close_input(&self->fmt_ctx);
    self->fmt_ctx = NULL;
  };
  return SCM_UNSPECIFIED;
}

size_t free_format_context(SCM scm_self)
{
  struct format_context_t *self = get_self(scm_self);
  format_context_destroy(scm_self);
  scm_gc_free(self, sizeof(struct format_context_t), "format-context");
  return 0;
}

AVCodecContext *open_codec(SCM scm_self, struct format_context_t *self, SCM scm_file_name, int stream_idx, const char *media_type)
{
  AVCodecContext *dec_ctx = self->fmt_ctx->streams[stream_idx]->codec;
  AVCodec *dec = avcodec_find_decoder(dec_ctx->codec_id);
  if (!dec) {
    format_context_destroy(scm_self);
    scm_misc_error("open-format-context", "Failed to find ~a codec for file '~a'",
                   scm_list_2(scm_from_locale_string(media_type), scm_file_name));
  };
  av_opt_set_int(dec_ctx, "refcounted_frames", 1, 0);
  if (avcodec_open2(dec_ctx, dec, NULL) < 0) {
    format_context_destroy(scm_self);
    scm_misc_error("open-format-context", "Failed to open ~a codec for file '~a'",
                   scm_list_2(scm_from_locale_string(media_type), scm_file_name));
  };
  return dec_ctx;
}

SCM open_format_context(SCM scm_file_name, SCM scm_debug)
{
  SCM retval;
  struct format_context_t *self;
  const char *file_name = scm_to_locale_string(scm_file_name);
  self = (struct format_context_t *)scm_gc_calloc(sizeof(struct format_context_t), "format-context");
  self->video_stream_idx = -1;
  self->audio_stream_idx = -1;
  SCM_NEWSMOB(retval, format_context_tag, self);

  int err;
  err = avformat_open_input(&self->fmt_ctx, file_name, NULL, NULL);
  if (err < 0) {
    format_context_destroy(retval);
    scm_misc_error("open-format-context", "Error opening file '~a': ~a", scm_list_2(scm_file_name, get_error_text(err)));
  };

  err = avformat_find_stream_info(self->fmt_ctx, NULL);
  if (err < 0) {
    format_context_destroy(retval);
    scm_misc_error("open-format-context", "No stream information in file '~a': ~a", scm_list_2(scm_file_name, get_error_text(err)));
  };

  self->video_stream_idx = av_find_best_stream(self->fmt_ctx, AVMEDIA_TYPE_VIDEO, -1, -1, NULL, 0);
  if (self->video_stream_idx >= 0)
    self->video_dec_ctx = open_codec(retval, self, scm_file_name, self->video_stream_idx, "video");

  self->audio_stream_idx = av_find_best_stream(self->fmt_ctx, AVMEDIA_TYPE_AUDIO, -1, -1, NULL, 0);
  if (self->audio_stream_idx >= 0)
    self->audio_dec_ctx = open_codec(retval, self, scm_file_name, self->audio_stream_idx, "audio");

  if (scm_is_true(scm_debug)) av_dump_format(self->fmt_ctx, 0, file_name, 0);

  self->frame = av_frame_alloc();

  av_init_packet(&self->pkt);
  self->pkt.data = NULL;
  self->pkt.size = 0;
  return retval;
}

SCM format_context_shape(SCM scm_self)
{
  struct format_context_t *self = get_self(scm_self);
  if (!self->video_dec_ctx)
    scm_misc_error("format-context-shape", "File format does not have a video stream", SCM_EOL);
  int width = self->video_dec_ctx->width;
  int height = self->video_dec_ctx->height;
  return scm_list_2(scm_from_int(width), scm_from_int(height));
}

SCM format_context_frame_rate(SCM scm_self)
{
  struct format_context_t *self = get_self(scm_self);
  if (self->video_stream_idx < 0)
    scm_misc_error("format-context-frame-rate", "File format does not have a video stream", SCM_EOL);
  AVRational avg_frame_rate = self->fmt_ctx->streams[self->video_stream_idx]->avg_frame_rate;
  return scm_divide(scm_from_int(avg_frame_rate.num), scm_from_int(avg_frame_rate.den));
}

SCM format_context_video_pts(SCM scm_self)
{
  struct format_context_t *self = get_self(scm_self);
  if (self->video_stream_idx < 0)
    scm_misc_error("format-context-video-pts", "File format does not have a video stream", SCM_EOL);
  AVRational time_base = self->fmt_ctx->streams[self->video_stream_idx]->time_base;
  int64_t video_pts = self->video_pts;
  return scm_divide(scm_product(scm_from_int(video_pts), scm_from_int(time_base.num)), scm_from_int(time_base.den));
}

SCM format_context_read_video(SCM scm_self)
{
  SCM retval = SCM_BOOL_F;

  scm_assert_smob_type(format_context_tag, scm_self);
  struct format_context_t *self = (struct format_context_t *)SCM_SMOB_DATA(scm_self);

  int got_frame = 0;
  while (!got_frame) {
    if (self->pkt.size <= 0) {
      if (self->orig_pkt.data) {
        av_free_packet(&self->orig_pkt);
        self->orig_pkt.data = NULL;
        self->orig_pkt.size = 0;
      };
      int err = av_read_frame(self->fmt_ctx, &self->pkt);
      if (err >= 0)
        self->orig_pkt = self->pkt;
      else if (err == AVERROR_EOF) {
        self->pkt.data = NULL;
        self->pkt.size = 0;
      } else
        scm_misc_error("format-context-read-video", "Error reading frame: ~a", scm_list_1(get_error_text(err)));
    };

    int decoded = self->pkt.size;
    if (self->pkt.stream_index == self->video_stream_idx) {
      int err = avcodec_decode_video2(self->video_dec_ctx, self->frame, &got_frame, &self->pkt);
      if (err < 0)
        scm_misc_error("format-context-read-video", "Error decoding frame: ~a", scm_list_1(get_error_text(err)));
      if (self->pkt.size <= 0 && !got_frame) break;
    };

    if (self->pkt.data) {
      self->pkt.data += decoded;
      self->pkt.size -= decoded;
    };
  };

  if (got_frame) {
    int offsets[AV_NUM_DATA_POINTERS];
    offsets_from_pointers(self->frame->data, offsets, AV_NUM_DATA_POINTERS);

#ifdef HAVE_BEST_EFFORT_TIMESTAMP
    self->video_pts = av_frame_get_best_effort_timestamp(self->frame);
#else
    if (self->frame->pkt_pts != AV_NOPTS_VALUE)
      self->video_pts = self->frame->pkt_pts;
    else if (self->frame->pkt_dts != AV_NOPTS_VALUE)
      self->video_pts = self->frame->pkt_dts;
    else
      self->video_pts = 0;
#endif

    int size = avpicture_get_size(self->frame->format, self->frame->width, self->frame->height);

    retval = scm_list_n(scm_from_int(self->frame->format),
                        scm_list_2(scm_from_int(self->frame->width), scm_from_int(self->frame->height)),
                        from_non_zero_array(offsets, AV_NUM_DATA_POINTERS, 1),
                        from_non_zero_array(self->frame->linesize, AV_NUM_DATA_POINTERS, 1),
                        scm_from_int(size),
                        scm_from_pointer(*self->frame->data, NULL),
                        SCM_UNDEFINED);
  } else
    retval = SCM_BOOL_F;
  return retval;
}

SCM format_context_channels(SCM scm_self)
{
  struct format_context_t *self = get_self(scm_self);
  if (self->audio_stream_idx < 0)
    scm_misc_error("format-context-channels", "File format does not have an audio stream", SCM_EOL);
  return scm_from_int(self->audio_dec_ctx->channels);
}

SCM format_context_rate(SCM scm_self)
{
  struct format_context_t *self = get_self(scm_self);
  // TODO: check for audio stream, refactor
  return scm_from_int(self->audio_dec_ctx->sample_rate);
}

SCM format_context_typecode(SCM scm_self)
{
  struct format_context_t *self = get_self(scm_self);
  return scm_from_int(self->audio_dec_ctx->sample_fmt);
}

void init_ffmpeg(void)
{
  format_context_tag = scm_make_smob_type("format-context", sizeof(struct format_context_t));
  scm_set_smob_free(format_context_tag, free_format_context);
  av_register_all();
  scm_c_define("AV_SAMPLE_FMT_U8" ,scm_from_int(AV_SAMPLE_FMT_U8 ));
  scm_c_define("AV_SAMPLE_FMT_S16",scm_from_int(AV_SAMPLE_FMT_S16));
  scm_c_define("AV_SAMPLE_FMT_S32",scm_from_int(AV_SAMPLE_FMT_S32));
  scm_c_define("AV_SAMPLE_FMT_FLT",scm_from_int(AV_SAMPLE_FMT_FLT));
  scm_c_define("AV_SAMPLE_FMT_DBL",scm_from_int(AV_SAMPLE_FMT_DBL));
  scm_c_define_gsubr("open-format-context", 2, 0, 0, open_format_context);
  scm_c_define_gsubr("format-context-shape", 1, 0, 0, format_context_shape);
  scm_c_define_gsubr("format-context-frame-rate", 1, 0, 0, format_context_frame_rate);
  scm_c_define_gsubr("format-context-read-video", 1, 0, 0, format_context_read_video);
  scm_c_define_gsubr("format-context-video-pts", 1, 0, 0, format_context_video_pts);
  scm_c_define_gsubr("format-context-channels", 1, 0, 0, format_context_channels);
  scm_c_define_gsubr("format-context-rate", 1, 0, 0, format_context_rate);
  scm_c_define_gsubr("format-context-typecode", 1, 0, 0, format_context_typecode);
}
