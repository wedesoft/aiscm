// AIscm - Guile extension for numerical arrays and tensors.
// Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018, 2019 Jan Wedekind <jan@wedesoft.de>
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
#include <stdio.h>
#include <libguile.h>
#include <libavutil/imgutils.h>
#include <libavutil/opt.h>
#include <libavformat/avformat.h>
#include "config.h"
#include "ffmpeg-helpers.h"
#include "ringbuffer.h"
#include "util-helpers.h"


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

#ifndef HAVE_AV_PACKET_UNREF
#define av_packet_unref av_free_packet
#endif

#define PIX_FMT AV_PIX_FMT_YUV420P

#ifndef AV_CODEC_CAP_VARIABLE_FRAME_SIZE
#define AV_CODEC_CAP_VARIABLE_FRAME_SIZE CODEC_CAP_VARIABLE_FRAME_SIZE
#endif

#ifndef AV_CODEC_FLAG_GLOBAL_HEADER
#define AV_CODEC_FLAG_GLOBAL_HEADER CODEC_FLAG_GLOBAL_HEADER
#endif

#ifndef AV_INPUT_BUFFER_MIN_SIZE
#define AV_INPUT_BUFFER_MIN_SIZE FF_MIN_BUFFER_SIZE
#endif


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
  AVFrame *video_target_frame;
  AVFrame *audio_packed_frame;
  AVFrame *audio_target_frame;
  int samples_count;
  struct ringbuffer_t audio_buffer;
};

static SCM get_error_text(int err)
{
  static char buf[255];
  av_strerror(err, buf, sizeof(buf));
  return scm_from_locale_string(buf);
}

static struct ffmpeg_t *get_self_no_check(SCM scm_self)
{
  return (struct ffmpeg_t *)SCM_SMOB_DATA(scm_self);
}

static struct ffmpeg_t *get_self(SCM scm_self)
{
  scm_assert_smob_type(ffmpeg_tag, scm_self);
  return get_self_no_check(scm_self);
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

static void write_frame(struct ffmpeg_t *self, AVPacket *packet, AVCodecContext *codec, AVStream *stream, int stream_idx)
{
#ifdef HAVE_AV_PACKET_RESCALE_TS
  av_packet_rescale_ts(packet, codec->time_base, stream->time_base);
#else
  if (codec->coded_frame->pts != AV_NOPTS_VALUE)
    packet->pts = av_rescale_q(codec->coded_frame->pts, codec->time_base, stream->time_base);
#endif
  packet->stream_index = stream_idx;
  int err = av_interleaved_write_frame(self->fmt_ctx, packet);
  if (err < 0)
    scm_misc_error("write-frame", "Error writing video frame: ~a",
                   scm_list_1(get_error_text(err)));
}

static int encode_video(struct ffmpeg_t *self, AVFrame *video_frame)
{
  AVCodecContext *codec = video_codec_ctx(self);

  // Initialise data packet
  AVPacket pkt = { 0 };
  av_init_packet(&pkt);

  // Encode the video frame
  int got_packet;
  int err = avcodec_encode_video2(codec, &pkt, video_frame, &got_packet);
  if (err < 0)
    scm_misc_error("encode-video", "Error encoding video frame: ~a",
                   scm_list_1(get_error_text(err)));

  // Write any new video packets
  if (got_packet)
    write_frame(self, &pkt, codec, video_stream(self), self->video_stream_idx);

  return got_packet;
}

static int encode_audio(struct ffmpeg_t *self, AVFrame *audio_frame)
{
  AVCodecContext *codec = audio_codec_ctx(self);

  // Initialise data packet
  AVPacket pkt = { 0 };
  av_init_packet(&pkt);

  // Encode the audio frame
  int got_packet;
  int err = avcodec_encode_audio2(codec, &pkt, audio_frame, &got_packet);
  if (err < 0)
    scm_misc_error("encode-audio", "Error encoding audio frame: ~a",
                   scm_list_1(get_error_text(err)));

  // Write any new audio packets
  if (got_packet)
    write_frame(self, &pkt, codec, audio_stream(self), self->audio_stream_idx);

  return got_packet;
}

SCM ffmpeg_destroy(SCM scm_self)
{
  struct ffmpeg_t *self = get_self_no_check(scm_self);

  if (self->header_written) {
    // Clear audio encoder pipeline
    if (self->audio_codec_ctx)
      while (encode_audio(self, NULL));

    // Clear video encoder pipeline
    if (self->video_codec_ctx)
      while (encode_video(self, NULL));
  };

  if (self->video_target_frame) {
    av_frame_unref(self->video_target_frame);
    av_frame_free(&self->video_target_frame);
    self->video_target_frame = NULL;
  };

  if (self->audio_packed_frame) {
    av_frame_unref(self->audio_packed_frame);
    av_frame_free(&self->audio_packed_frame);
    self->audio_packed_frame = NULL;
  };

  if (self->audio_target_frame) {
    av_frame_unref(self->audio_target_frame);
    av_frame_free(&self->audio_target_frame);
    self->audio_target_frame = NULL;
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
    av_packet_unref(&self->orig_pkt);
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
  struct ffmpeg_t *self = get_self_no_check(scm_self);
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
  scm_dynwind_begin(0);
  const char *file_name = scm_to_locale_string(scm_file_name);
  scm_dynwind_free(file_name);
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
  self->video_target_frame = allocate_frame(retval);
  self->audio_target_frame = allocate_frame(retval);

  // Initialise data packet
  av_init_packet(&self->pkt);
  self->pkt.data = NULL;
  self->pkt.size = 0;

  scm_dynwind_end();
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
}

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
  retval->width = scm_to_int(scm_cadr(scm_shape));
  retval->height = scm_to_int(scm_car(scm_shape));

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
    SCM scm_select_rate, SCM scm_channels, SCM scm_audio_bit_rate, SCM scm_select_format)
{
  // Get codec context
  AVCodecContext *retval = audio_stream->codec;
  const AVCodec *codec = retval->codec;

  // Select sample format
  SCM scm_sample_formats = SCM_EOL;
  if (codec->sample_fmts) {
    int i;
    for (i=0; codec->sample_fmts[i] != AV_SAMPLE_FMT_NONE; i++)
      scm_sample_formats = scm_cons(scm_from_int(codec->sample_fmts[i]), scm_sample_formats);
  };
  SCM scm_sample_format = clean_up_on_failure(scm_self, ffmpeg_destroy, scm_select_format, scm_sample_formats);

  // Set sample format
  retval->sample_fmt = scm_to_int(scm_sample_format);

  // Select sample rate
  SCM scm_rates = SCM_EOL;
  if (codec->supported_samplerates) {
    int i;
    for (i=0; codec->supported_samplerates[i] != 0; i++)
      scm_rates = scm_cons(scm_from_int(codec->supported_samplerates[i]), scm_rates);
  };
  SCM scm_rate = clean_up_on_failure(scm_self, ffmpeg_destroy, scm_select_rate, scm_rates);

  // Set sample rate
  retval->sample_rate = scm_to_int(scm_rate);
  audio_stream->time_base.num = 1;
  audio_stream->time_base.den = retval->sample_rate;
  retval->time_base = audio_stream->time_base;

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

static AVFrame *allocate_output_audio_frame(SCM scm_self, AVCodecContext *audio_codec, enum AVSampleFormat sample_fmt)
{
  AVFrame *retval = allocate_frame(scm_self);
  retval->format = sample_fmt;
  retval->channel_layout = audio_codec->channel_layout;
  retval->sample_rate = audio_codec->sample_rate;

  if (audio_codec->codec->capabilities & AV_CODEC_CAP_VARIABLE_FRAME_SIZE)
    retval->nb_samples = 2 * AV_INPUT_BUFFER_MIN_SIZE;
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
  scm_dynwind_begin(0);
  const char *file_name = scm_to_locale_string(scm_file_name);
  scm_dynwind_free(file_name);
  self = (struct ffmpeg_t *)scm_gc_calloc(sizeof(struct ffmpeg_t), "ffmpeg");
  self->video_stream_idx = -1;
  self->audio_stream_idx = -1;
  SCM_NEWSMOB(retval, ffmpeg_tag, self);

  int err;
  const char *format_name = NULL;
  if (!scm_is_false(scm_format_name)) {
    format_name = scm_to_locale_string(scm_symbol_to_string(scm_format_name));
    scm_dynwind_free(format_name);
  };
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
        self->video_codec_ctx->flags |= AV_CODEC_FLAG_GLOBAL_HEADER;

    // Open output video codec
    open_codec(retval, self->video_codec_ctx, video_encoder, "video", scm_file_name);

    // Allocate frame
    self->video_target_frame = allocate_output_video_frame(retval, self->video_codec_ctx);
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
    SCM scm_select_format  = scm_cadddr(scm_audio_parameters);

    // Configure the output audio codec
    self->audio_codec_ctx =
      configure_output_audio_codec(retval, audio_stream, audio_codec_id,
                                   scm_select_rate, scm_channels, scm_audio_bit_rate, scm_select_format);

    // Some formats want stream headers to be separate.
    if (self->fmt_ctx->oformat->flags & AVFMT_GLOBALHEADER)
        self->audio_codec_ctx->flags |= AV_CODEC_FLAG_GLOBAL_HEADER;

    // Open output audio codec
    open_codec(retval, self->audio_codec_ctx, audio_encoder, "audio", scm_file_name);

    // Allocate audio frame
    self->audio_target_frame =
      allocate_output_audio_frame(retval, self->audio_codec_ctx, self->audio_codec_ctx->sample_fmt);
    self->audio_packed_frame =
      allocate_output_audio_frame(retval, self->audio_codec_ctx, av_get_packed_sample_fmt(self->audio_codec_ctx->sample_fmt));

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

  scm_dynwind_end();
  return retval;
}

SCM ffmpeg_have_video(SCM scm_self)
{
  struct ffmpeg_t *self = get_self(scm_self);
  return scm_from_bool(self->video_codec_ctx);
}

SCM ffmpeg_have_audio(SCM scm_self)
{
  struct ffmpeg_t *self = get_self(scm_self);
  return scm_from_bool(self->audio_codec_ctx);
}

SCM ffmpeg_shape(SCM scm_self)
{
  AVCodecContext *ctx = video_codec_ctx(get_self(scm_self));
  return scm_list_2(scm_from_int(ctx->height), scm_from_int(ctx->width));
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
  if (self->audio_buffer.buffer) ringbuffer_flush(&self->audio_buffer);
  return scm_self;
}

static void read_packet(struct ffmpeg_t *self)
{
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
}

static void consume_packet_data(AVPacket *pkt, int decoded)
{
  pkt->data += decoded;
  pkt->size -= decoded;
}

static int64_t frame_timestamp(AVFrame *frame)
{
  int64_t retval;
#ifdef HAVE_AV_FRAME_PTS
  if (frame->pts != AV_NOPTS_VALUE)
    retval = frame->pts;
#else
  if (frame->pkt_pts != AV_NOPTS_VALUE)
    retval = frame->pkt_pts;
#endif
  else if (frame->pkt_dts != AV_NOPTS_VALUE)
    retval = frame->pkt_dts;
  else
    retval = 0;
  return retval;
}

static SCM list_audio_frame_info(struct ffmpeg_t *self, AVFrame *frame)
{
  int channels = audio_codec_ctx(self)->channels;
  int64_t offsets[AV_NUM_DATA_POINTERS];
  offsets_from_pointers(frame->data, offsets, AV_NUM_DATA_POINTERS);
  int frame_size =
    av_samples_get_buffer_size(NULL, channels, frame->nb_samples, frame->format, 1);

  return scm_list_n(scm_from_int(frame->format),
                    scm_list_2(scm_from_int(frame->nb_samples), scm_from_int(channels)),
                    scm_from_int(self->audio_codec_ctx->sample_rate),
                    from_non_zero_array(offsets, AV_NUM_DATA_POINTERS, 1),
                    scm_from_pointer(*frame->data, NULL),
                    scm_from_int(frame_size),
                    SCM_UNDEFINED);
}

static SCM list_timestamped_audio(struct ffmpeg_t *self, AVFrame *frame)
{
  return scm_list_2(scm_from_locale_symbol("audio"),
                    scm_product(scm_from_int(frame_timestamp(frame)), time_base(audio_stream(self))));
}

static SCM decode_audio(struct ffmpeg_t *self, AVPacket *pkt, AVFrame *frame)
{
  int got_frame;
  int len = avcodec_decode_audio4(self->audio_codec_ctx, frame, &got_frame, pkt);
  if (len < 0)
    scm_misc_error("ffmpeg-decode-audio/video", "Error decoding frame: ~a", scm_list_1(get_error_text(len)));
  consume_packet_data(pkt, FFMIN(pkt->size, len));
  return got_frame ? list_timestamped_audio(self, frame) : SCM_BOOL_F;
}

static SCM list_video_frame_info(struct ffmpeg_t *self, AVFrame *frame)
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
                    scm_list_2(scm_from_int(frame->height), scm_from_int(frame->width)),
                    from_non_zero_array(offsets, AV_NUM_DATA_POINTERS, 1),
                    from_non_zero_array(linesize, AV_NUM_DATA_POINTERS, 1),
                    scm_from_pointer(*frame->data, NULL),
                    scm_from_int(size),
                    SCM_UNDEFINED);
}

static void make_frame_writable(AVFrame *frame)
{
#ifdef HAVE_AV_FRAME_MAKE_WRITABLE
  // Make frame writeable
  int err = av_frame_make_writable(frame);
  if (err < 0)
    scm_misc_error("make-frame-writable", "Error making frame writeable: ~a",
                   scm_list_1(get_error_text(err)));
#endif
}

SCM ffmpeg_target_video_frame(SCM scm_self)
{
  struct ffmpeg_t *self = get_self(scm_self);
  if (!is_input_context(self))
    make_frame_writable(self->video_target_frame);
  return list_video_frame_info(self, self->video_target_frame);
}

SCM ffmpeg_target_audio_frame(SCM scm_self)
{
  struct ffmpeg_t *self = get_self(scm_self);
  if (!is_input_context(self))
    make_frame_writable(self->audio_target_frame);
  return list_audio_frame_info(self, self->audio_target_frame);
}

SCM ffmpeg_packed_audio_frame(SCM scm_self)
{
  struct ffmpeg_t *self = get_self(scm_self);
  return list_audio_frame_info(self, self->audio_packed_frame);
}

static SCM list_timestamped_video(struct ffmpeg_t *self, AVFrame *frame)
{
  return scm_list_2(scm_from_locale_symbol("video"),
                    scm_product(scm_from_int(frame_timestamp(frame)), time_base(video_stream(self))));
}

static SCM decode_video(struct ffmpeg_t *self, AVPacket *pkt, AVFrame *frame)
{
  int got_frame;
  int len = avcodec_decode_video2(self->video_codec_ctx, frame, &got_frame, pkt);
  if (len < 0)
    scm_misc_error("ffmpeg-decode-audio/video", "Error decoding frame: ~a", scm_list_1(get_error_text(len)));
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

SCM ffmpeg_decode_audio_video(SCM scm_self)
{
  SCM retval = SCM_BOOL_F;

  struct ffmpeg_t *self = get_self(scm_self);

  if (!is_input_context(self))
    scm_misc_error("ffmpeg-decode-audio/video", "Attempt to read frame from FFmpeg output video", SCM_EOL);

  while (scm_is_false(retval)) {
    if (packet_empty(self)) read_packet(self);

    int reading_cache = packet_empty(self);

    if (self->pkt.stream_index == self->audio_stream_idx) {
      av_frame_unref(self->audio_target_frame);
      retval = decode_audio(self, &self->pkt, self->audio_target_frame);
    } else if (self->pkt.stream_index == self->video_stream_idx) {
      av_frame_unref(self->video_target_frame);
      retval = decode_video(self, &self->pkt, self->video_target_frame);
    } else
      consume_packet_data(&self->pkt, self->pkt.size);

    if (scm_is_false(retval) && reading_cache) break;
  };

  return retval;
}

SCM ffmpeg_encode_video(SCM scm_self)
{
  // TODO: AVFMT_RAWPICTURE
  struct ffmpeg_t *self = get_self(scm_self);
  if (is_input_context(self))
    scm_misc_error("ffmpeg-encode-video", "Attempt to write to FFmpeg input video", SCM_EOL);

  // Set frame timestamp
  self->video_target_frame->pts = self->output_video_pts++;

  encode_video(self, self->video_target_frame);

  return SCM_UNSPECIFIED;
}

SCM ffmpeg_audio_buffer_fill(SCM scm_self)
{
  struct ffmpeg_t *self = get_self(scm_self);
  return scm_from_int(self->audio_buffer.fill);
}

SCM ffmpeg_buffer_audio(SCM scm_self, SCM scm_data, SCM scm_bytes)
{
  struct ffmpeg_t *self = get_self(scm_self);
  ringbuffer_store(&self->audio_buffer, scm_to_pointer(scm_data), scm_to_int(scm_bytes));
  return SCM_UNSPECIFIED;
}

static void fetch_buffered_audio_data(char *data, int count, int offset, void *userdata)
{
  memcpy((char *)userdata + offset, data, count);
}

SCM ffmpeg_fetch_audio(SCM scm_self, SCM scm_data, SCM scm_bytes)
{
  struct ffmpeg_t *self = get_self(scm_self);
  ringbuffer_fetch(&self->audio_buffer, scm_to_int(scm_bytes), fetch_buffered_audio_data, scm_to_pointer(scm_data));
  return SCM_UNSPECIFIED;
}

SCM ffmpeg_encode_audio(SCM scm_self)
{
  struct ffmpeg_t *self = get_self(scm_self);
  AVCodecContext *codec = audio_codec_ctx(self);
  AVFrame *audio_frame = self->audio_target_frame;
  audio_frame->pts = av_rescale_q(self->samples_count, (AVRational){1, codec->sample_rate}, codec->time_base);
  self->samples_count += audio_frame->nb_samples;

  encode_audio(self, self->audio_target_frame);
  return SCM_UNSPECIFIED;
}

SCM ffmpeg_crop_audio_frame_size(SCM scm_self, SCM scm_size)
{
  struct ffmpeg_t *self = get_self(scm_self);
  self->audio_target_frame->nb_samples = scm_to_int(scm_size);
  self->audio_packed_frame->nb_samples = scm_to_int(scm_size);
  return SCM_UNSPECIFIED;
}

void init_ffmpeg(void)
{
  av_register_all();
  avformat_network_init();
  ffmpeg_tag = scm_make_smob_type("ffmpeg", sizeof(struct ffmpeg_t));
  scm_set_smob_free(ffmpeg_tag, free_ffmpeg);
  scm_c_define_gsubr("make-ffmpeg-input"           , 2, 0, 0, SCM_FUNC(make_ffmpeg_input           ));
  scm_c_define_gsubr("make-ffmpeg-output"          , 7, 0, 0, SCM_FUNC(make_ffmpeg_output          ));
  scm_c_define_gsubr("ffmpeg-have-video?"          , 1, 0, 0, SCM_FUNC(ffmpeg_have_video           ));
  scm_c_define_gsubr("ffmpeg-have-audio?"          , 1, 0, 0, SCM_FUNC(ffmpeg_have_audio           ));
  scm_c_define_gsubr("ffmpeg-shape"                , 1, 0, 0, SCM_FUNC(ffmpeg_shape                ));
  scm_c_define_gsubr("ffmpeg-destroy"              , 1, 0, 0, SCM_FUNC(ffmpeg_destroy              ));
  scm_c_define_gsubr("ffmpeg-frame-rate"           , 1, 0, 0, SCM_FUNC(ffmpeg_frame_rate           ));
  scm_c_define_gsubr("ffmpeg-video-bit-rate"       , 1, 0, 0, SCM_FUNC(ffmpeg_video_bit_rate       ));
  scm_c_define_gsubr("ffmpeg-aspect-ratio"         , 1, 0, 0, SCM_FUNC(ffmpeg_aspect_ratio         ));
  scm_c_define_gsubr("ffmpeg-channels"             , 1, 0, 0, SCM_FUNC(ffmpeg_channels             ));
  scm_c_define_gsubr("ffmpeg-rate"                 , 1, 0, 0, SCM_FUNC(ffmpeg_rate                 ));
  scm_c_define_gsubr("ffmpeg-typecode"             , 1, 0, 0, SCM_FUNC(ffmpeg_typecode             ));
  scm_c_define_gsubr("ffmpeg-decode-audio/video"   , 1, 0, 0, SCM_FUNC(ffmpeg_decode_audio_video   ));
  scm_c_define_gsubr("ffmpeg-target-video-frame"   , 1, 0, 0, SCM_FUNC(ffmpeg_target_video_frame   ));
  scm_c_define_gsubr("ffmpeg-target-audio-frame"   , 1, 0, 0, SCM_FUNC(ffmpeg_target_audio_frame   ));
  scm_c_define_gsubr("ffmpeg-packed-audio-frame"   , 1, 0, 0, SCM_FUNC(ffmpeg_packed_audio_frame   ));
  scm_c_define_gsubr("ffmpeg-encode-video"         , 1, 0, 0, SCM_FUNC(ffmpeg_encode_video         ));
  scm_c_define_gsubr("ffmpeg-audio-buffer-fill"    , 1, 0, 0, SCM_FUNC(ffmpeg_audio_buffer_fill    ));
  scm_c_define_gsubr("ffmpeg-buffer-audio"         , 3, 0, 0, SCM_FUNC(ffmpeg_buffer_audio         ));
  scm_c_define_gsubr("ffmpeg-fetch-audio"          , 3, 0, 0, SCM_FUNC(ffmpeg_fetch_audio          ));
  scm_c_define_gsubr("ffmpeg-encode-audio"         , 1, 0, 0, SCM_FUNC(ffmpeg_encode_audio         ));
  scm_c_define_gsubr("ffmpeg-seek"                 , 2, 0, 0, SCM_FUNC(ffmpeg_seek                 ));
  scm_c_define_gsubr("ffmpeg-flush"                , 1, 0, 0, SCM_FUNC(ffmpeg_flush                ));
  scm_c_define_gsubr("ffmpeg-crop-audio-frame-size", 2, 0, 0, SCM_FUNC(ffmpeg_crop_audio_frame_size));
}
