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
#include <libavutil/imgutils.h>
#include <libavutil/opt.h>
#include <libavformat/avformat.h>
#include "config.h"
#include "ffmpeg-helpers.h"
#include "ringbuffer.h"
#include "image-helpers.h"


// http://dranger.com/ffmpeg/
// https://github.com/FFmpeg/FFmpeg/tree/n2.6.9/doc/examples
// https://raw.githubusercontent.com/FFmpeg/FFmpeg/n2.6.9/doc/examples/demuxing_decoding.c
// https://raw.githubusercontent.com/FFmpeg/FFmpeg/n2.6.9/doc/examples/filtering_video.c
// https://raw.githubusercontent.com/FFmpeg/FFmpeg/n2.6.9/doc/examples/muxing.c

#ifndef HAVE_FRAME_ALLOC
#define av_frame_alloc avcodec_alloc_frame
#define av_frame_free avcodec_free_frame
#define av_frame_unref avcodec_get_frame_defaults
#endif

#define PIX_FMT AV_PIX_FMT_YUV420P

static scm_t_bits ffmpeg_tag;

struct ffmpeg_t {
  AVFormatContext *fmt_ctx;
  AVCodecContext *video_codec_ctx;
  AVCodecContext *audio_codec_ctx;
  int video_stream_idx;
  int audio_stream_idx;
  long output_video_pts;
  char header_written;
  char output_file;
  AVPacket pkt;
  AVPacket orig_pkt;
  AVFrame *video_frame;
  AVFrame *audio_frame;
  struct ringbuffer_t audio_buffer;
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

static AVCodecContext *video_codec_ctx(struct ffmpeg_t *self)
{
  if (!self->video_codec_ctx)
    scm_misc_error("video-codec-ctx", "File format does not have a video stream", SCM_EOL);
  return self->video_codec_ctx;
}

static AVCodecContext *audio_codec_ctx(struct ffmpeg_t *self)
{
  if (!self->audio_codec_ctx)
    scm_misc_error("audio-codec-ctx", "File format does not have an audio stream", SCM_EOL);
  return self->audio_codec_ctx;
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

static char is_input_context(struct ffmpeg_t *self)
{
  return self->fmt_ctx->iformat != NULL;
}

SCM ffmpeg_destroy(SCM scm_self)
{
  struct ffmpeg_t *self = get_self(scm_self);
  if (self->video_frame) {
    av_frame_unref(self->video_frame);
    av_frame_free(&self->video_frame);
    self->video_frame = NULL;
  };
  if (self->audio_frame) {
    av_frame_unref(self->audio_frame);
    av_frame_free(&self->audio_frame);
    self->audio_frame = NULL;
  };
  if (self->audio_buffer.buffer) {
    ringbuffer_destroy(&self->audio_buffer);
    self->audio_buffer.buffer = NULL;
  };
  if (self->header_written) {
    av_write_trailer(self->fmt_ctx);
    self->header_written = 0;
  };
  if (self->orig_pkt.data) {
    av_free_packet(&self->orig_pkt);
    self->orig_pkt.data = NULL;
  };
  if (self->audio_codec_ctx) {
    avcodec_close(self->audio_codec_ctx);
    self->audio_codec_ctx = NULL;
  };
  if (self->video_codec_ctx) {
    avcodec_close(self->video_codec_ctx);
    self->video_codec_ctx = NULL;
  };
  if (self->output_file) {
    avio_close(self->fmt_ctx->pb);
    self->output_file = 0;
  };
  if (self->fmt_ctx) {
    if (is_input_context(self))
      avformat_close_input(&self->fmt_ctx);
    else
      avformat_free_context(self->fmt_ctx);
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

static AVCodecContext *open_codec(SCM scm_self, AVCodecContext *codec_ctx, AVCodec *codec,
                                  const char *media_type, SCM scm_file_name)
{
  int err = avcodec_open2(codec_ctx, codec, NULL);
  if (err < 0) {
    ffmpeg_destroy(scm_self);
    scm_misc_error("open-codec", "Failed to open ~a codec for file '~a'",
                   scm_list_2(scm_from_locale_string(media_type), scm_file_name));
  };
  return codec_ctx;
}

static AVCodecContext *open_decoder(SCM scm_self, SCM scm_file_name,
                                    AVStream *stream, const char *media_type)
{
  AVCodecContext *dec_ctx = stream->codec;
  AVCodec *decoder = avcodec_find_decoder(dec_ctx->codec_id);
  if (!decoder) {
    ffmpeg_destroy(scm_self);
    scm_misc_error("open-codec", "Failed to find ~a codec for file '~a'",
                   scm_list_2(scm_from_locale_string(media_type), scm_file_name));
  };
  return open_codec(scm_self, dec_ctx, decoder, media_type, scm_file_name);
}

static AVFrame *allocate_frame(SCM scm_self)
{
  AVFrame *retval = av_frame_alloc();
  if (!retval) {
    ffmpeg_destroy(scm_self);
    scm_misc_error("allocate-frame", "Error allocating frame", SCM_EOL);
  };
  memset(retval, 0, sizeof(AVFrame));
  return retval;
}

SCM make_ffmpeg_input(SCM scm_file_name, SCM scm_debug)
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
    scm_misc_error("make-ffmpeg-input", "Error opening file '~a': ~a", scm_list_2(scm_file_name, get_error_text(err)));
  };

  err = avformat_find_stream_info(self->fmt_ctx, NULL);
  if (err < 0) {
    ffmpeg_destroy(retval);
    scm_misc_error("make-ffmpeg-input", "No stream information in file '~a': ~a", scm_list_2(scm_file_name, get_error_text(err)));
  };

  // TODO: only open desired streams
  // Open video stream
  self->video_stream_idx = av_find_best_stream(self->fmt_ctx, AVMEDIA_TYPE_VIDEO, -1, -1, NULL, 0);
  if (self->video_stream_idx >= 0)
    self->video_codec_ctx = open_decoder(retval, scm_file_name, video_stream(self), "video");

  // Open audio stream
  self->audio_stream_idx = av_find_best_stream(self->fmt_ctx, AVMEDIA_TYPE_AUDIO, -1, -1, NULL, 0);
  if (self->audio_stream_idx >= 0)
    self->audio_codec_ctx = open_decoder(retval, scm_file_name, audio_stream(self), "audio");

  // Print debug information
  if (scm_is_true(scm_debug)) av_dump_format(self->fmt_ctx, 0, file_name, 0);

  // Allocate input frames
  self->video_frame = allocate_frame(retval);
  self->audio_frame = allocate_frame(retval);

  // Initialise data packet
  av_init_packet(&self->pkt);
  self->pkt.data = NULL;
  self->pkt.size = 0;

  return retval;
}

static AVCodec *find_encoder(SCM scm_self, enum AVCodecID codec_id, const char *output_type)
{
  if (codec_id == AV_CODEC_ID_NONE) {// TODO: test (needs wav or mp3 container selection above first)
    ffmpeg_destroy(scm_self);
    scm_misc_error("make-ffmpeg-output", "File format does not support ~a encoding",
                   scm_list_1(scm_from_locale_string(output_type)));
  };

  AVCodec *retval = avcodec_find_encoder(codec_id);// TODO: autodetect or select video codec
  if (!retval) {
    ffmpeg_destroy(scm_self);
    scm_misc_error("make-ffmpeg-output", "Error finding encoder for codec '~a'",
                   scm_list_1(scm_from_locale_string(avcodec_descriptor_get(codec_id)->name)));
  };

  return retval;
}

static AVStream *open_output_stream(SCM scm_self, AVCodec *encoder, int *stream_idx, const char *output_type, SCM scm_file_name)
{
  struct ffmpeg_t *self = get_self(scm_self);
  AVStream *retval = avformat_new_stream(self->fmt_ctx, encoder);
  if (!retval) {
    ffmpeg_destroy(scm_self);
    scm_misc_error("make-ffmpeg-output", "Error allocating ~a stream for file '~a'",
                   scm_list_2(scm_from_locale_string(output_type), scm_file_name));
  };
  retval->id = self->fmt_ctx->nb_streams - 1;
  *stream_idx = retval->id;
  return retval;
};

static AVCodecContext *configure_output_video_codec(AVStream *video_stream, enum AVCodecID video_codec_id,
    SCM scm_video_bit_rate, SCM scm_shape, SCM scm_frame_rate, SCM scm_aspect_ratio)
{
  // Get codec context
  AVCodecContext *retval = video_stream->codec;

  // Set codec id
  retval->codec_id = video_codec_id;
  retval->codec_type = AVMEDIA_TYPE_VIDEO;

  // Set encoder bit rate
  retval->bit_rate = scm_to_int(scm_video_bit_rate);

  // Set video frame width and height
  retval->width = scm_to_int(scm_car(scm_shape));
  retval->height = scm_to_int(scm_cadr(scm_shape));

  // Set video frame rate
  video_stream->avg_frame_rate.num = scm_to_int(scm_numerator(scm_frame_rate));
  video_stream->avg_frame_rate.den = scm_to_int(scm_denominator(scm_frame_rate));
  video_stream->time_base.num = video_stream->avg_frame_rate.den;
  video_stream->time_base.den = video_stream->avg_frame_rate.num;
  retval->time_base = video_stream->time_base;

  // Set intra frame lower limit
  retval->gop_size = 12;

  // Set pixel format
  retval->pix_fmt = PIX_FMT;

  if (retval->codec_id == AV_CODEC_ID_MPEG1VIDEO) retval->mb_decision = 2;

  // Set aspect ratio
  video_stream->sample_aspect_ratio.num = scm_to_int(scm_numerator(scm_aspect_ratio));
  video_stream->sample_aspect_ratio.den = scm_to_int(scm_denominator(scm_aspect_ratio));
  retval->sample_aspect_ratio = video_stream->sample_aspect_ratio;

  return retval;
}

static AVCodecContext *configure_output_audio_codec(SCM scm_self, AVStream *audio_stream, enum AVCodecID audio_codec_id,
    SCM scm_select_rate, SCM scm_channels, SCM scm_audio_bit_rate, SCM scm_sample_format)
{
  // Get codec context
  AVCodecContext *retval = audio_stream->codec;

  // Set sample format, TODO: use parameter
  retval->sample_fmt = scm_to_int(scm_sample_format);

  // Select sample rate
  const AVCodec *codec = retval->codec;
  SCM scm_rates = SCM_EOL; 
  int i;
  for (i=0; codec->supported_samplerates[i]; i++)
    scm_rates = scm_cons(scm_from_int(codec->supported_samplerates[i]), scm_rates);
  SCM scm_rate = clean_up_on_failure(scm_self, ffmpeg_destroy, scm_select_rate, scm_rates);

  // Set sample rate
  retval->sample_rate = scm_to_int(scm_rate);
  audio_stream->time_base.num = 1;
  audio_stream->time_base.den = retval->sample_rate;

  // Set channels
  retval->channels = scm_to_int(scm_channels);
  retval->channel_layout = av_get_default_channel_layout(retval->channels);

  // Set bit rate
  retval->bit_rate = scm_to_int(scm_audio_bit_rate);

  return retval;
}

static AVFrame *allocate_output_video_frame(SCM scm_self, AVCodecContext *video_context)
{
  AVFrame *retval = allocate_frame(scm_self);
  int width = video_context->width;
  int height = video_context->height;
  retval->format = PIX_FMT;
  retval->width = width;
  retval->height = height;
#ifdef HAVE_AV_FRAME_GET_BUFFER
  int err = av_frame_get_buffer(retval, 32);
  if (err < 0) {
    ffmpeg_destroy(scm_self);
    scm_misc_error("allocate-output-frame", "Error allocating frame buffer: ~a",
                   scm_list_1(get_error_text(err)));
  };
#else
  int size = avpicture_get_size(PIX_FMT, width, height);
  uint8_t *frame_buffer = (uint8_t *)av_malloc(size);
  if (!frame_buffer) {
    ffmpeg_destroy(scm_self);
    scm_misc_error("allocate-output-video-frame", "Error allocating video frame memory", SCM_EOL);
  };
  avpicture_fill((AVPicture *)retval, frame_buffer, PIX_FMT, width, height);
#endif
  return retval;
}

static AVFrame *allocate_output_audio_frame(SCM scm_self, AVCodecContext *audio_codec)
{
  AVFrame *retval = allocate_frame(scm_self);
  retval->format = audio_codec->sample_fmt;
  retval->channel_layout = audio_codec->channel_layout;
  retval->sample_rate = audio_codec->sample_rate;

  if (audio_codec->codec->capabilities & CODEC_CAP_VARIABLE_FRAME_SIZE)
    retval->nb_samples = 2 * FF_MIN_BUFFER_SIZE;
  else
    retval->nb_samples = audio_codec->frame_size;

#ifdef HAVE_AV_FRAME_GET_BUFFER
  int err = av_frame_get_buffer(retval, 0);
  if (err < 0) {
    ffmpeg_destroy(scm_self);
    scm_misc_error("allocate-output-audio-frame", "Error allocating audio frame memory", SCM_EOL);
  };
#else
  int channels = av_get_channel_layout_nb_channels(retval->channel_layout);
  int err = av_samples_alloc(retval->data, &retval->linesize[0], channels, retval->nb_samples, retval->format, 0);
  // TODO: need av_freep?
  if (err < 0) {
    ffmpeg_destroy(scm_self);
    scm_misc_error("allocate-output-audio-frame", "Could not allocate audio buffer", SCM_EOL);
  };
#endif
  return retval;
}

SCM make_ffmpeg_output(SCM scm_file_name,
                       SCM scm_format_name,
                       SCM scm_video_parameters,
                       SCM scm_have_video,
                       SCM scm_audio_parameters,
                       SCM scm_have_audio,
                       SCM scm_debug)
{
  SCM retval;
  struct ffmpeg_t *self;
  const char *file_name = scm_to_locale_string(scm_file_name);
  self = (struct ffmpeg_t *)scm_gc_calloc(sizeof(struct ffmpeg_t), "ffmpeg");
  self->video_stream_idx = -1;
  self->audio_stream_idx = -1;
  SCM_NEWSMOB(retval, ffmpeg_tag, self);

  int err;
  const char *format_name = NULL;
  if (!scm_is_false(scm_format_name)) format_name = scm_to_locale_string(scm_symbol_to_string(scm_format_name));
#ifdef HAVE_AVFORMAT_ALLOC_OUTPUT_CONTEXT2
  err = avformat_alloc_output_context2(&self->fmt_ctx, NULL, format_name, file_name);
  if (!self->fmt_ctx) {
    ffmpeg_destroy(retval);
    scm_misc_error("make-ffmpeg-output", "Error initializing output format for file '~a': ~a",
                   scm_list_2(scm_file_name, get_error_text(err)));
  };
#else
  AVOutputFormat *format;
  if (format_name)
    format = av_guess_format(format_name, NULL, NULL);
  else
    format = av_guess_format(NULL, file_name, NULL);
  if (!format) {
    ffmpeg_destroy(retval);
    scm_misc_error("make-ffmpeg-output", "Unable to determine file format for file '~a'",
                   scm_list_1(scm_file_name));
  };
  self->fmt_ctx = avformat_alloc_context();
  if (!self->fmt_ctx) {
    ffmpeg_destroy(retval);
    scm_misc_error("make-ffmpeg-output", "Error initializing output format for file '~a'",
                   scm_list_1(scm_file_name));
  };
  self->fmt_ctx->oformat = format;
  strncpy(self->fmt_ctx->filename, file_name, sizeof(self->fmt_ctx->filename));
#endif

  char have_video = scm_is_true(scm_have_video);
  if (have_video) {
    // Open codec and video stream
    enum AVCodecID video_codec_id = self->fmt_ctx->oformat->video_codec;
    AVCodec *video_encoder = find_encoder(retval, video_codec_id, "video");
    AVStream *video_stream = open_output_stream(retval, video_encoder, &self->video_stream_idx, "video", scm_file_name);

    // Get video parameters
    SCM scm_shape          = scm_car(scm_video_parameters);
    SCM scm_frame_rate     = scm_cadr(scm_video_parameters);
    SCM scm_video_bit_rate = scm_caddr(scm_video_parameters);
    SCM scm_aspect_ratio   = scm_cadddr(scm_video_parameters);

    // Configure the output video codec
    self->video_codec_ctx =
      configure_output_video_codec(video_stream, video_codec_id, scm_video_bit_rate, scm_shape, scm_frame_rate, scm_aspect_ratio);

    // Some formats want stream headers to be separate.
    if (self->fmt_ctx->oformat->flags & AVFMT_GLOBALHEADER)
        self->video_codec_ctx->flags |= CODEC_FLAG_GLOBAL_HEADER;

    // Open output video codec
    open_codec(retval, self->video_codec_ctx, video_encoder, "video", scm_file_name);

    // Allocate frame
    self->video_frame = allocate_output_video_frame(retval, self->video_codec_ctx);
  };

  char have_audio = scm_is_true(scm_have_audio);
  if (have_audio) {
    // Open audio codec and stream
    enum AVCodecID audio_codec_id = self->fmt_ctx->oformat->audio_codec;
    AVCodec *audio_encoder = find_encoder(retval, audio_codec_id, "audio");
    AVStream *audio_stream = open_output_stream(retval, audio_encoder, &self->audio_stream_idx, "audio", scm_file_name);

    // Get audio parameters
    SCM scm_select_rate    = scm_car(scm_audio_parameters);
    SCM scm_channels       = scm_cadr(scm_audio_parameters);
    SCM scm_audio_bit_rate = scm_caddr(scm_audio_parameters);
    SCM scm_sample_format  = scm_cadddr(scm_audio_parameters);

    // Configure the output audio codec
    self->audio_codec_ctx =
      configure_output_audio_codec(retval, audio_stream, audio_codec_id,
                                   scm_select_rate, scm_channels, scm_audio_bit_rate, scm_sample_format);

    // Some formats want stream headers to be separate.
    if (self->fmt_ctx->oformat->flags & AVFMT_GLOBALHEADER)
        self->audio_codec_ctx->flags |= CODEC_FLAG_GLOBAL_HEADER;

    // Open output audio codec
    open_codec(retval, self->audio_codec_ctx, audio_encoder, "audio", scm_file_name);

    // Allocate audio frame
    self->audio_frame = allocate_output_audio_frame(retval, self->audio_codec_ctx);

    // Initialise audio buffer
    ringbuffer_init(&self->audio_buffer, 1024);
  };

  if (scm_is_true(scm_debug)) av_dump_format(self->fmt_ctx, 0, file_name, 1);

  // Open the output file if needed
  if (!(self->fmt_ctx->oformat->flags & AVFMT_NOFILE)) {
    int err = avio_open(&self->fmt_ctx->pb, file_name, AVIO_FLAG_WRITE);
    if (err < 0) {
      ffmpeg_destroy(retval);
      scm_misc_error("make-ffmpeg-output", "Could not open '~a': ~a",
                     scm_list_2(scm_file_name, get_error_text(err)));
    }
    self->output_file = 1;
  }

  // Write video file header
  err = avformat_write_header(self->fmt_ctx, NULL);
  if (err < 0) {
    ffmpeg_destroy(retval);
    scm_misc_error("make-ffmpeg-output", "Error writing header of video '~a': ~a",
                   scm_list_2(scm_file_name, get_error_text(err)));
  };
  self->header_written = 1;

  return retval;
}

SCM ffmpeg_shape(SCM scm_self)
{
  AVCodecContext *ctx = video_codec_ctx(get_self(scm_self));
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

SCM ffmpeg_video_bit_rate(SCM scm_self)
{
  return scm_from_int(video_codec_ctx(get_self(scm_self))->bit_rate);
}

SCM ffmpeg_aspect_ratio(SCM scm_self)
{
  AVRational aspect_ratio = video_stream(get_self(scm_self))->sample_aspect_ratio;
  return rational(aspect_ratio.num, aspect_ratio.den);
}

SCM ffmpeg_seek(SCM scm_self, SCM scm_position)
{
  struct ffmpeg_t *self = get_self(scm_self);
  if (!is_input_context(self))
    scm_misc_error("ffmpeg-seek", "Attempt to seek in FFmpeg output video", SCM_EOL);

  int64_t position = (int64_t)(scm_to_double(scm_position) * AV_TIME_BASE);
  av_seek_frame(self->fmt_ctx, -1, position, AVSEEK_FLAG_ANY);// TODO: check error
  return scm_position;
}

SCM ffmpeg_flush(SCM scm_self)
{
  struct ffmpeg_t *self = get_self(scm_self);
  if (self->video_codec_ctx) avcodec_flush_buffers(self->video_codec_ctx);
  if (self->audio_codec_ctx) avcodec_flush_buffers(self->audio_codec_ctx);
  return scm_self;
}

static void read_packet(struct ffmpeg_t *self)
{
  if (self->orig_pkt.data) {
    av_free_packet(&self->orig_pkt);
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
}

static void consume_packet_data(AVPacket *pkt, int decoded)
{
  pkt->data += decoded;
  pkt->size -= decoded;
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

static SCM list_timestamped_audio(struct ffmpeg_t *self, AVFrame *frame)
{
  int data_size = av_get_bytes_per_sample(self->audio_codec_ctx->sample_fmt);
  int channels = self->audio_codec_ctx->channels;
  int nb_samples = frame->nb_samples;
  void *ptr = scm_gc_malloc_pointerless(nb_samples * channels * data_size, "aiscm audio frame");
  pack_audio(frame->data, channels, nb_samples, data_size, ptr);

  return scm_list_n(scm_from_locale_symbol("audio"),
                    scm_product(scm_from_int(frame_timestamp(frame)), time_base(audio_stream(self))),
                    scm_from_int(self->audio_codec_ctx->sample_fmt),
                    scm_list_2(scm_from_int(channels), scm_from_int(nb_samples)),
                    scm_from_pointer(ptr, NULL),
                    scm_from_int(data_size * channels * nb_samples),
                    SCM_UNDEFINED);
}

static SCM decode_audio(struct ffmpeg_t *self, AVPacket *pkt, AVFrame *frame)
{
  int got_frame;
  int len = avcodec_decode_audio4(self->audio_codec_ctx, frame, &got_frame, pkt);
  if (len < 0)
    scm_misc_error("ffmpeg-read-audio/video", "Error decoding frame: ~a", scm_list_1(get_error_text(len)));
  consume_packet_data(pkt, FFMIN(pkt->size, len));
  return got_frame ? list_timestamped_audio(self, frame) : SCM_BOOL_F;
}

SCM list_video_frame_info(struct ffmpeg_t *self, AVFrame *frame)
{
  // note that the pointer offsets can be negative for FFmpeg frames because av_frame_get_buffer
  // allocates separate memory locations for each image plane.
  int64_t offsets[AV_NUM_DATA_POINTERS];
  offsets_from_pointers(frame->data, offsets, AV_NUM_DATA_POINTERS);

  int64_t linesize[AV_NUM_DATA_POINTERS];
  int_array_to_long(linesize, frame->linesize, AV_NUM_DATA_POINTERS);

#ifdef HAVE_IMAGE_BUFFER_SIZE
  int size = av_image_get_buffer_size(frame->format, frame->width, frame->height, 32);
#else
  int size = avpicture_get_size(frame->format, frame->width, frame->height);
#endif

  return scm_list_n(scm_from_int(frame->format),
                    scm_list_2(scm_from_int(frame->width), scm_from_int(frame->height)),
                    from_non_zero_array(offsets, AV_NUM_DATA_POINTERS, 1),
                    from_non_zero_array(linesize, AV_NUM_DATA_POINTERS, 1),
                    scm_from_pointer(*frame->data, NULL),
                    scm_from_int(size),
                    SCM_UNDEFINED);
}

SCM ffmpeg_target_video_frame(SCM scm_self)
{
  struct ffmpeg_t *self = get_self(scm_self);
  if (is_input_context(self))
    scm_misc_error("ffmpeg-seek", "Attempt to write to FFmpeg input video", SCM_EOL);

#ifdef HAVE_AV_FRAME_MAKE_WRITABLE
  // Make frame writeable
  int err = av_frame_make_writable(self->video_frame);
  if (err < 0)
    scm_misc_error("ffmpeg-target-video-frame", "Error making frame writeable: ~a",
                   scm_list_1(get_error_text(err)));
#endif

  return list_video_frame_info(self, self->video_frame);
}

static SCM list_timestamped_video(struct ffmpeg_t *self, AVFrame *frame)
{
  return scm_append(scm_list_2(scm_list_2(scm_from_locale_symbol("video"),
                                          scm_product(scm_from_int(frame_timestamp(frame)), time_base(video_stream(self)))),
                               list_video_frame_info(self, frame)));
}

static SCM decode_video(struct ffmpeg_t *self, AVPacket *pkt, AVFrame *frame)
{
  int got_frame;
  int len = avcodec_decode_video2(self->video_codec_ctx, frame, &got_frame, pkt);
  if (len < 0)
    scm_misc_error("ffmpeg-read-audio/video", "Error decoding frame: ~a", scm_list_1(get_error_text(len)));
  consume_packet_data(pkt, pkt->size);
  return got_frame ? list_timestamped_video(self, frame) : SCM_BOOL_F;
}

static int packet_empty(struct ffmpeg_t *self)
{
  return self->pkt.size <= 0;
}

SCM ffmpeg_channels(SCM scm_self)
{
  return scm_from_int(audio_codec_ctx(get_self(scm_self))->channels);
}

SCM ffmpeg_rate(SCM scm_self)
{
  return scm_from_int(audio_codec_ctx(get_self(scm_self))->sample_rate);
}

SCM ffmpeg_typecode(SCM scm_self)
{
  return scm_from_int(audio_codec_ctx(get_self(scm_self))->sample_fmt);
}

SCM ffmpeg_read_audio_video(SCM scm_self)
{
  SCM retval = SCM_BOOL_F;

  struct ffmpeg_t *self = get_self(scm_self);

  if (!is_input_context(self))
    scm_misc_error("ffmpeg-read-audio/video", "Attempt to read frame from FFmpeg output video", SCM_EOL);


  while (scm_is_false(retval)) {
    if (packet_empty(self)) read_packet(self);

    int reading_cache = packet_empty(self);

    if (self->pkt.stream_index == self->audio_stream_idx) {
      av_frame_unref(self->audio_frame);
      retval = decode_audio(self, &self->pkt, self->audio_frame);
    } else if (self->pkt.stream_index == self->video_stream_idx) {
      av_frame_unref(self->video_frame);
      retval = decode_video(self, &self->pkt, self->video_frame);
    } else
      consume_packet_data(&self->pkt, self->pkt.size);

    if (scm_is_false(retval) && reading_cache) break;
  };

  return retval;
}

SCM ffmpeg_write_video(SCM scm_self)
{
  // TODO: AVFMT_RAWPICTURE
  struct ffmpeg_t *self = get_self(scm_self);
  if (is_input_context(self))
    scm_misc_error("ffmpeg-write-video", "Attempt to write to FFmpeg input video", SCM_EOL);

  // Set frame timestamp
  self->video_frame->pts = self->output_video_pts++;

  AVCodecContext *codec = video_codec_ctx(self);

  // Initialise data packet
  AVPacket pkt = { 0 };
  av_init_packet(&pkt);

  // Encode the video frame
  int got_packet;
  int err = avcodec_encode_video2(codec, &pkt, self->video_frame, &got_packet);
  if (err < 0)
    scm_misc_error("ffmpeg-write-video", "Error encoding video frame: ~a",
                   scm_list_1(get_error_text(err)));

  // Write any new video packets
  if (got_packet) {
#ifdef HAVE_AV_PACKET_RESCALE_TS
    av_packet_rescale_ts(&pkt, codec->time_base, video_stream(self)->time_base);
#else
    if (codec->coded_frame->pts != AV_NOPTS_VALUE)
      pkt.pts = av_rescale_q(codec->coded_frame->pts, codec->time_base, video_stream(self)->time_base);
#endif
    pkt.stream_index = self->video_stream_idx;
    err = av_interleaved_write_frame(self->fmt_ctx, &pkt);
    if (err < 0)
      scm_misc_error("ffmpeg-write-video", "Error writing video frame: ~a",
                     scm_list_1(get_error_text(err)));
  };

  return SCM_UNSPECIFIED;
}

SCM ffmpeg_write_audio(SCM scm_self, SCM scm_data, SCM scm_bytes)
{
  struct ffmpeg_t *self = get_self(scm_self);
  ringbuffer_store(&self->audio_buffer, scm_to_pointer(scm_data), scm_to_int(scm_bytes));
  printf("audio buffer bytes = %d\n", self->audio_buffer.fill);
  printf("encoding buffer samples = %d\n", self->audio_frame->nb_samples);
  printf("sample format = %d\n", self->audio_codec_ctx->sample_fmt);
  return SCM_UNSPECIFIED;
}

void init_ffmpeg(void)
{
  av_register_all();
  avformat_network_init();
  ffmpeg_tag = scm_make_smob_type("ffmpeg", sizeof(struct ffmpeg_t));
  scm_set_smob_free(ffmpeg_tag, free_ffmpeg);
  scm_c_define_gsubr("make-ffmpeg-input", 2, 0, 0, make_ffmpeg_input);
  scm_c_define_gsubr("make-ffmpeg-output", 7, 0, 0, make_ffmpeg_output);
  scm_c_define_gsubr("ffmpeg-shape", 1, 0, 0, ffmpeg_shape);
  scm_c_define_gsubr("ffmpeg-destroy", 1, 0, 0, ffmpeg_destroy);
  scm_c_define_gsubr("ffmpeg-frame-rate", 1, 0, 0, ffmpeg_frame_rate);
  scm_c_define_gsubr("ffmpeg-video-bit-rate", 1, 0, 0, ffmpeg_video_bit_rate);
  scm_c_define_gsubr("ffmpeg-aspect-ratio", 1, 0, 0, ffmpeg_aspect_ratio);
  scm_c_define_gsubr("ffmpeg-channels", 1, 0, 0, ffmpeg_channels);
  scm_c_define_gsubr("ffmpeg-rate", 1, 0, 0, ffmpeg_rate);
  scm_c_define_gsubr("ffmpeg-typecode", 1, 0, 0, ffmpeg_typecode);
  scm_c_define_gsubr("ffmpeg-read-audio/video", 1, 0, 0, ffmpeg_read_audio_video);
  scm_c_define_gsubr("ffmpeg-target-video-frame", 1, 0, 0, ffmpeg_target_video_frame);
  scm_c_define_gsubr("ffmpeg-write-video", 1, 0, 0, ffmpeg_write_video);
  scm_c_define_gsubr("ffmpeg-write-audio", 3, 0, 0, ffmpeg_write_audio);
  scm_c_define_gsubr("ffmpeg-seek", 2, 0, 0, ffmpeg_seek);
  scm_c_define_gsubr("ffmpeg-flush", 1, 0, 0, ffmpeg_flush);
}
