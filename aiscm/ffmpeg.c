#include <libguile.h>
#include <libavutil/opt.h>
#include <libavformat/avformat.h>
#include "config.h"
#include "helpers.h"

// http://dranger.com/ffmpeg/
// https://github.com/FFmpeg/FFmpeg/blob/n2.6.9/doc/examples/demuxing_decoding.c
// https://github.com/FFmpeg/FFmpeg/blob/n2.6.9/doc/examples/filtering_video.c

#ifndef HAVE_FRAME_ALLOC
#warning "Using old FFmpeg methods for frame allocation"
#define av_frame_alloc avcodec_alloc_frame
#define av_frame_free avcodec_free_frame
#endif

#ifndef HAVE_PACKET_UNREF
#warning "av_packet_unref not supported"
#define av_packet_unref av_free_packet
#endif

static scm_t_bits ffmpeg_tag;

struct ffmpeg_t {
  AVFormatContext *fmt_ctx;
  AVCodecContext *video_dec_ctx;
  AVCodecContext *audio_dec_ctx;
  int video_stream_idx;
  int audio_stream_idx;
  AVPacket pkt;
  AVPacket orig_pkt;
  AVFrame *frame;
  int64_t video_pts;
  int64_t audio_pts;
};

static SCM get_error_text(int err)
{
  static char buf[255];
  av_strerror(err, buf, sizeof(buf));
  return scm_from_locale_string(buf);
}

static struct ffmpeg_t *get_self(SCM scm_self)
{
  scm_assert_smob_type(ffmpeg_tag, scm_self);
  return (struct ffmpeg_t *)SCM_SMOB_DATA(scm_self);
}

static AVCodecContext *video_dec_ctx(struct ffmpeg_t *self)
{
  if (!self->video_dec_ctx)
    scm_misc_error("video-dec-ctx", "File format does not have a video stream", SCM_EOL);
  return self->video_dec_ctx;
}

static AVCodecContext *audio_dec_ctx(struct ffmpeg_t *self)
{
  if (!self->audio_dec_ctx)
    scm_misc_error("audio-dec-ctx", "File format does not have an audio stream", SCM_EOL);
  return self->audio_dec_ctx;
}

static AVStream *video_stream(struct ffmpeg_t *self)
{
  if (self->video_stream_idx < 0)
    scm_misc_error("video-stream", "File format does not have a video stream", SCM_EOL);
  return self->fmt_ctx->streams[self->video_stream_idx];
}

static AVStream *audio_stream(struct ffmpeg_t *self)
{
  if (self->audio_stream_idx < 0)
    scm_misc_error("audio-stream", "File format does not have an audio stream", SCM_EOL);
  return self->fmt_ctx->streams[self->audio_stream_idx];
}

SCM ffmpeg_destroy(SCM scm_self)
{
  scm_assert_smob_type(ffmpeg_tag, scm_self);
  struct ffmpeg_t *self = get_self(scm_self);
  if (self->frame) {
    av_frame_free(&self->frame);
    self->frame = NULL;
  };
  if (self->orig_pkt.data) {
    av_packet_unref(&self->orig_pkt);
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

size_t free_ffmpeg(SCM scm_self)
{
  struct ffmpeg_t *self = get_self(scm_self);
  ffmpeg_destroy(scm_self);
  scm_gc_free(self, sizeof(struct ffmpeg_t), "ffmpeg");
  return 0;
}

static AVCodecContext *open_codec(SCM scm_self, struct ffmpeg_t *self, SCM scm_file_name,
                                  int stream_idx, const char *media_type)
{
  AVCodecContext *dec_ctx = self->fmt_ctx->streams[stream_idx]->codec;
  AVCodec *dec = avcodec_find_decoder(dec_ctx->codec_id);
  if (!dec) {
    ffmpeg_destroy(scm_self);
    scm_misc_error("open-codec", "Failed to find ~a codec for file '~a'",
                   scm_list_2(scm_from_locale_string(media_type), scm_file_name));
  };
  av_opt_set_int(dec_ctx, "refcounted_frames", 1, 0);
  if (avcodec_open2(dec_ctx, dec, NULL) < 0) {
    ffmpeg_destroy(scm_self);
    scm_misc_error("open-codec", "Failed to open ~a codec for file '~a'",
                   scm_list_2(scm_from_locale_string(media_type), scm_file_name));
  };
  return dec_ctx;
}

SCM open_ffmpeg(SCM scm_file_name, SCM scm_debug)
{
  SCM retval;
  struct ffmpeg_t *self;
  const char *file_name = scm_to_locale_string(scm_file_name);
  self = (struct ffmpeg_t *)scm_gc_calloc(sizeof(struct ffmpeg_t), "ffmpeg");
  self->video_stream_idx = -1;
  self->audio_stream_idx = -1;
  SCM_NEWSMOB(retval, ffmpeg_tag, self);

  int err;
  err = avformat_open_input(&self->fmt_ctx, file_name, NULL, NULL);
  if (err < 0) {
    ffmpeg_destroy(retval);
    scm_misc_error("open-ffmpeg", "Error opening file '~a': ~a", scm_list_2(scm_file_name, get_error_text(err)));
  };

  err = avformat_find_stream_info(self->fmt_ctx, NULL);
  if (err < 0) {
    ffmpeg_destroy(retval);
    scm_misc_error("open-ffmpeg", "No stream information in file '~a': ~a", scm_list_2(scm_file_name, get_error_text(err)));
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

SCM ffmpeg_shape(SCM scm_self)
{
  AVCodecContext *ctx = video_dec_ctx(get_self(scm_self));
  return scm_list_2(scm_from_int(ctx->width), scm_from_int(ctx->height));
}

static SCM rational(int numerator, int denominator)
{
  return scm_divide(scm_from_int(numerator), scm_from_int(denominator));
}

static SCM time_base(AVStream *stream)
{
  AVRational time_base = stream->time_base;
  return rational(time_base.num, time_base.den);
}

SCM ffmpeg_frame_rate(SCM scm_self)
{
  AVRational avg_frame_rate = video_stream(get_self(scm_self))->avg_frame_rate;
  return rational(avg_frame_rate.num, avg_frame_rate.den);
}

SCM ffmpeg_video_pts(SCM scm_self)
{
  struct ffmpeg_t *self = get_self(scm_self);
  return scm_product(scm_from_int(self->video_pts), time_base(video_stream(self)));
}

SCM ffmpeg_audio_pts(SCM scm_self)
{
  struct ffmpeg_t *self = get_self(scm_self);
  return scm_product(scm_from_int(self->audio_pts), time_base(audio_stream(self)));
}

static void read_packet(struct ffmpeg_t *self)
{
  if (self->pkt.size <= 0) {
    if (self->orig_pkt.data) {
      av_packet_unref(&self->orig_pkt);
      self->orig_pkt.data = NULL;
      self->orig_pkt.size = 0;
    };
    int err = av_read_frame(self->fmt_ctx, &self->pkt);
    if (err >= 0)
      self->orig_pkt = self->pkt;
    else {
      self->pkt.data = NULL;
      self->pkt.size = 0;
      if (err != AVERROR_EOF)
        scm_misc_error("read-packet", "Error reading frame: ~a", scm_list_1(get_error_text(err)));
    };
  };
}

static int decode_video(struct ffmpeg_t *self, int *got_frame)
{
  int err = avcodec_decode_video2(self->video_dec_ctx, self->frame, got_frame, &self->pkt);
  if (err < 0)
    scm_misc_error("ffmpeg-read-video", "Error decoding frame: ~a", scm_list_1(get_error_text(err)));
  return self->pkt.size;
}

static int decode_audio(struct ffmpeg_t *self, int *got_frame)
{
  int len = avcodec_decode_audio4(self->audio_dec_ctx, self->frame, got_frame, &self->pkt);
  if (len < 0)
    scm_misc_error("ffmpeg-read-audio", "Error decoding frame: ~a", scm_list_1(get_error_text(len)));
  return FFMIN(self->pkt.size, len);
}

static int64_t frame_timestamp(AVFrame *frame)
{
  int64_t retval;
  if (frame->pkt_pts != AV_NOPTS_VALUE)
    retval = frame->pkt_pts;
  else if (frame->pkt_dts != AV_NOPTS_VALUE)
    retval = frame->pkt_dts;
  else
    retval = 0;
  return retval;
}

static SCM picture_information(struct ffmpeg_t *self)
{
  self->video_pts = frame_timestamp(self->frame);

  int offsets[AV_NUM_DATA_POINTERS];
  offsets_from_pointers(self->frame->data, offsets, AV_NUM_DATA_POINTERS);
  int size = avpicture_get_size(self->frame->format, self->frame->width, self->frame->height);

  return scm_list_n(scm_from_locale_symbol("video"),
                    scm_from_int(self->frame->format),
                    scm_list_2(scm_from_int(self->frame->width), scm_from_int(self->frame->height)),
                    from_non_zero_array(offsets, AV_NUM_DATA_POINTERS, 1),
                    from_non_zero_array(self->frame->linesize, AV_NUM_DATA_POINTERS, 1),
                    scm_from_pointer(*self->frame->data, NULL),
                    scm_from_int(size),
                    SCM_UNDEFINED);
}

static SCM samples_information(struct ffmpeg_t *self)
{
  self->audio_pts = frame_timestamp(self->frame);

  int data_size = av_get_bytes_per_sample(self->audio_dec_ctx->sample_fmt);
  int channels = self->audio_dec_ctx->channels;
  int nb_samples = self->frame->nb_samples;
  void *ptr = scm_gc_malloc_pointerless(nb_samples * channels * data_size, "aiscm audio frame");
  pack_audio(self->frame->data, channels, nb_samples, data_size, ptr);

  return scm_list_5(scm_from_locale_symbol("audio"),
                    scm_from_int(self->audio_dec_ctx->sample_fmt),
                    scm_list_2(scm_from_int(channels), scm_from_int(nb_samples)),
                    scm_from_pointer(ptr, NULL),
                    scm_from_int(data_size * channels * nb_samples));
}

SCM ffmpeg_read_audio_video(SCM scm_self, SCM scm_do_audio, SCM scm_do_video)
{
  SCM retval = SCM_BOOL_F;

  struct ffmpeg_t *self = get_self(scm_self);

  int got_frame = 0;

  while (!got_frame) {
    read_packet(self);

    int decoded;
    if (self->pkt.stream_index == self->audio_stream_idx && scm_is_true(scm_do_audio)) {
      decoded = decode_audio(self, &got_frame);
      if (got_frame) retval = samples_information(self);
    } else if (self->pkt.stream_index == self->video_stream_idx && scm_is_true(scm_do_video)) {
      decoded = decode_video(self, &got_frame);
      if (got_frame) retval = picture_information(self);
    } else
      decoded = self->pkt.size;

    if (!got_frame && self->pkt.size <= 0) break;

    if (self->pkt.data) {
      self->pkt.data += decoded;
      self->pkt.size -= decoded;
    };
  };

  return retval;
}

SCM ffmpeg_channels(SCM scm_self)
{
  return scm_from_int(audio_dec_ctx(get_self(scm_self))->channels);
}

SCM ffmpeg_rate(SCM scm_self)
{
  return scm_from_int(audio_dec_ctx(get_self(scm_self))->sample_rate);
}

SCM ffmpeg_typecode(SCM scm_self)
{
  return scm_from_int(audio_dec_ctx(get_self(scm_self))->sample_fmt);
}

void init_ffmpeg(void)
{
  ffmpeg_tag = scm_make_smob_type("ffmpeg", sizeof(struct ffmpeg_t));
  scm_set_smob_free(ffmpeg_tag, free_ffmpeg);
  av_register_all();
  avformat_network_init();
  scm_c_define("AV_SAMPLE_FMT_U8P" ,scm_from_int(AV_SAMPLE_FMT_U8P ));
  scm_c_define("AV_SAMPLE_FMT_S16P",scm_from_int(AV_SAMPLE_FMT_S16P));
  scm_c_define("AV_SAMPLE_FMT_S32P",scm_from_int(AV_SAMPLE_FMT_S32P));
  scm_c_define("AV_SAMPLE_FMT_FLTP",scm_from_int(AV_SAMPLE_FMT_FLTP));
  scm_c_define("AV_SAMPLE_FMT_DBLP",scm_from_int(AV_SAMPLE_FMT_DBLP));
  scm_c_define_gsubr("open-ffmpeg", 2, 0, 0, open_ffmpeg);
  scm_c_define_gsubr("ffmpeg-shape", 1, 0, 0, ffmpeg_shape);
  scm_c_define_gsubr("ffmpeg-frame-rate", 1, 0, 0, ffmpeg_frame_rate);
  scm_c_define_gsubr("ffmpeg-video-pts", 1, 0, 0, ffmpeg_video_pts);
  scm_c_define_gsubr("ffmpeg-channels", 1, 0, 0, ffmpeg_channels);
  scm_c_define_gsubr("ffmpeg-rate", 1, 0, 0, ffmpeg_rate);
  scm_c_define_gsubr("ffmpeg-typecode", 1, 0, 0, ffmpeg_typecode);
  scm_c_define_gsubr("ffmpeg-read-audio/video", 3, 0, 0, ffmpeg_read_audio_video);
  scm_c_define_gsubr("ffmpeg-audio-pts", 1, 0, 0, ffmpeg_audio_pts);
}
