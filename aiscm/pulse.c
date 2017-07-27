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
#include <pthread.h>
#include <pulse/pulseaudio.h>
#include <unistd.h>
#include <libguile.h>
#include "ringbuffer.h"
#include "util-helpers.h"


// https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/
// http://freedesktop.org/software/pulseaudio/doxygen/pacat-simple_8c-example.html
// https://jan.newmarch.name/LinuxSound/Sampled/PulseAudio/

static scm_t_bits pulsedev_tag;

struct pulsedev_t {
  pa_sample_spec sample_spec;
  struct ringbuffer_t audio_buffer;
  pa_threaded_mainloop *mainloop;
  pa_mainloop_api *mainloop_api;
  pa_context *context;
  pa_stream *stream;
};

static struct pulsedev_t *get_self_no_check(SCM scm_self)
{
  return (struct pulsedev_t *)SCM_SMOB_DATA(scm_self);
}

static struct pulsedev_t *get_self(SCM scm_self)
{
  scm_assert_smob_type(pulsedev_tag, scm_self);
  return get_self_no_check(scm_self);
}

static pa_threaded_mainloop *get_mainloop(struct pulsedev_t *self)
{
  if (!self->mainloop)
    scm_misc_error("pulsedev", "Device is not open. Did you call 'destroy' before?", SCM_EOL);
  return self->mainloop;
}

SCM pulsedev_destroy(SCM scm_self)
{
  struct pulsedev_t *self = get_self_no_check(scm_self);
  if (self->stream) {
    pa_stream_disconnect(self->stream);
    pa_stream_unref(self->stream);
    self->stream = NULL;
  };
  if (self->context) {
    pa_context_disconnect(self->context);
    pa_context_unref(self->context);
    self->context = NULL;
  };
  if (self->mainloop) {
    pa_threaded_mainloop_stop(self->mainloop);
    pa_threaded_mainloop_free(self->mainloop);
    self->mainloop = NULL;
    self->mainloop_api = NULL;
  };
  if (self->audio_buffer.buffer) {
    ringbuffer_destroy(&self->audio_buffer);
    self->audio_buffer.buffer = NULL;
  };
  return SCM_UNSPECIFIED;
}

size_t free_pulsedev(SCM scm_self)
{
  struct pulsedev_t *self = get_self_no_check(scm_self);
  pulsedev_destroy(scm_self);
  scm_gc_free(self, sizeof(struct pulsedev_t), "pulse");
  return 0;
}

static void write_from_ringbuffer(char *data, int count, int offset, void *userdata)
{
  pa_stream_write((pa_stream *)userdata, data, count, NULL, 0LL, PA_SEEK_RELATIVE);
}

static void stream_write_callback(pa_stream *s, size_t length, void *userdata) {
  struct pulsedev_t *self = (struct pulsedev_t *)userdata;
  if (self->audio_buffer.fill)
    ringbuffer_fetch(&self->audio_buffer, length, write_from_ringbuffer, self->stream);
  else
    pa_threaded_mainloop_signal(self->mainloop, 0);
}

static void stream_read_callback(pa_stream *s, size_t length, void *userdata) {
  struct pulsedev_t *self = (struct pulsedev_t *)userdata;
  while (pa_stream_readable_size(self->stream) > 0) {
    const void *data;
    size_t count;
    pa_stream_peek(s, &data, &count);
    ringbuffer_store(&self->audio_buffer, data, count);
    pa_stream_drop(self->stream);
  };
  pa_threaded_mainloop_signal(self->mainloop, 0);
}

static void initialise_mainloop(struct pulsedev_t *self)
{
  self->mainloop = pa_threaded_mainloop_new();
  self->mainloop_api = pa_threaded_mainloop_get_api(self->mainloop);
}

void context_state_callback(pa_context *context, void *userdata)
{
  *(pa_context_state_t *)userdata = pa_context_get_state(context);
}

static void initialise_context(struct pulsedev_t *self)
{
  self->context = pa_context_new(self->mainloop_api, "aiscm");
  pa_context_connect(self->context, NULL, 0, NULL);
  pa_context_state_t context_state = PA_CONTEXT_UNCONNECTED;
  pa_context_set_state_callback(self->context, context_state_callback, &context_state);
  if (pa_threaded_mainloop_start(self->mainloop) != PA_OK)
    scm_misc_error("make-pulsedev", "Error starting threaded mainloop: ~a",
                   scm_list_1(scm_from_locale_string(pa_strerror(pa_context_errno(self->context)))));

  while (context_state != PA_CONTEXT_READY)
    pa_threaded_mainloop_wait(self->mainloop);
}

static void initialise_stream(struct pulsedev_t *self, char playback)
{
  self->stream = pa_stream_new(self->context, "playback", &self->sample_spec, NULL);
  if (!self->stream)
    scm_misc_error("make-pulsedev", "Error creating audio stream: ~a",
                   scm_list_1(scm_from_locale_string(pa_strerror(pa_context_errno(self->context)))));
  if (playback)
    pa_stream_set_write_callback(self->stream, stream_write_callback, self);
  else
    pa_stream_set_read_callback(self->stream, stream_read_callback, self);
}

static void connect_stream(struct pulsedev_t *self, const char *name, char playback, pa_usec_t latency)
{
  static pa_stream_flags_t flags = PA_STREAM_ADJUST_LATENCY | PA_STREAM_INTERPOLATE_TIMING | PA_STREAM_AUTO_TIMING_UPDATE;
  pa_buffer_attr buffer_attr;
  memset(&buffer_attr, 0, sizeof(buffer_attr));
  buffer_attr.fragsize = pa_usec_to_bytes(latency, &self->sample_spec);
  buffer_attr.tlength = pa_usec_to_bytes(latency, &self->sample_spec);
  buffer_attr.maxlength = (uint32_t)-1;
  buffer_attr.minreq = pa_usec_to_bytes(0, &self->sample_spec);
  if (playback)
    pa_stream_connect_playback(self->stream, name, &buffer_attr, flags, NULL, NULL);
  else
    pa_stream_connect_record(self->stream, name, &buffer_attr, flags);
}

SCM make_pulsedev(SCM scm_name, SCM scm_type, SCM scm_playback, SCM scm_channels, SCM scm_rate, SCM scm_latency)
{
  SCM retval;
  struct pulsedev_t *self = (struct pulsedev_t *)scm_gc_calloc(sizeof(struct pulsedev_t), "pulsedev");
  SCM_NEWSMOB(retval, pulsedev_tag, self);

  const char *name = scm_is_string(scm_name) ? scm_to_locale_string(scm_name) : NULL;
  char playback = scm_is_true(scm_playback);
  pa_usec_t latency = (pa_usec_t)(scm_to_double(scm_latency) * 1e6);
  self->sample_spec.format = scm_to_int(scm_type);
  self->sample_spec.rate = scm_to_int(scm_rate);
  self->sample_spec.channels = scm_to_int(scm_channels);

  ringbuffer_init(&self->audio_buffer, 1024);
  initialise_mainloop(self);
  initialise_context(self);
  initialise_stream(self, playback);
  connect_stream(self, name, playback, latency);

  return retval;
}

SCM pulsedev_write(SCM scm_self, SCM scm_data, SCM scm_bytes)
{
  struct pulsedev_t *self = get_self(scm_self);
  pa_threaded_mainloop *mainloop = get_mainloop(self);
  pa_threaded_mainloop_lock(mainloop);
  ringbuffer_store(&self->audio_buffer, scm_to_pointer(scm_data), scm_to_int(scm_bytes));
  pa_threaded_mainloop_unlock(mainloop);
  return SCM_UNSPECIFIED;
}

void wait_for_flush(pa_stream *stream, int success, void *userdata)
{
  pa_threaded_mainloop_signal(userdata, 0);
}

SCM pulsedev_flush(SCM scm_self)// TODO: check audio device still open
{
  struct pulsedev_t *self = get_self(scm_self);
  pa_threaded_mainloop *mainloop = get_mainloop(self);
  pa_threaded_mainloop_lock(mainloop);
  ringbuffer_flush(&self->audio_buffer);
  pa_operation *operation = pa_stream_flush(self->stream, wait_for_flush, mainloop);
  while (pa_operation_get_state(operation) == PA_OPERATION_RUNNING)
    pa_threaded_mainloop_wait(mainloop);
  pa_operation_unref(operation);
  pa_threaded_mainloop_unlock(mainloop);
  return SCM_UNSPECIFIED;
}

useconds_t latency_usec(struct pulsedev_t *self)
{
  pa_threaded_mainloop *mainloop = get_mainloop(self);
  pa_threaded_mainloop_lock(mainloop);
  pa_usec_t ringbuffer_usec = pa_bytes_to_usec(self->audio_buffer.fill, &self->sample_spec);
  pa_usec_t pulse_usec;
  int negative;
  pa_stream_get_latency(self->stream, &pulse_usec, &negative);
  useconds_t retval =  negative ? ringbuffer_usec - pulse_usec : ringbuffer_usec + pulse_usec;
  pa_threaded_mainloop_unlock(mainloop);
  return retval;
}

SCM pulsedev_drain(SCM scm_self)
{
  struct pulsedev_t *self = get_self(scm_self);
  pa_threaded_mainloop *mainloop = get_mainloop(self);
  pa_threaded_mainloop_lock(mainloop);
  while (self->audio_buffer.fill > 0)
    pa_threaded_mainloop_wait(mainloop);
  useconds_t usecs_remaining = latency_usec(self);
  pa_threaded_mainloop_unlock(mainloop);
  usleep(usecs_remaining);
  return SCM_UNSPECIFIED;
}

SCM pulsedev_latency(SCM scm_self)
{
  struct pulsedev_t *self = get_self(scm_self);
  return scm_from_double(1e-6 * latency_usec(self));
}

static void fetch_callback(char *data, int count, int offset, void *userdata)
{
  memcpy(userdata + offset, data, count);
}

SCM pulsedev_read(SCM scm_self, SCM scm_bytes)// TODO: check audio device still open
{
  struct pulsedev_t *self = get_self(scm_self);
  pa_threaded_mainloop *mainloop = get_mainloop(self);
  pa_threaded_mainloop_lock(mainloop);
  int bytes = scm_to_int(scm_bytes);
  void *buffer = scm_gc_malloc_pointerless(bytes, "aiscm pulse frame");
  while (self->audio_buffer.fill < bytes)
    pa_threaded_mainloop_wait(mainloop);
  ringbuffer_fetch(&self->audio_buffer, bytes, fetch_callback, buffer);
  pa_threaded_mainloop_unlock(mainloop);
  return scm_from_pointer(buffer, NULL);
}

void init_pulse(void)
{
  pulsedev_tag = scm_make_smob_type("pulsedev", sizeof(struct pulsedev_t));
  scm_set_smob_free(pulsedev_tag, free_pulsedev);
  scm_c_define("PA_SAMPLE_U8"       , scm_from_int(PA_SAMPLE_U8       ));
  scm_c_define("PA_SAMPLE_S16LE"    , scm_from_int(PA_SAMPLE_S16LE    ));
  scm_c_define("PA_SAMPLE_S32LE"    , scm_from_int(PA_SAMPLE_S32LE    ));
  scm_c_define("PA_SAMPLE_FLOAT32LE", scm_from_int(PA_SAMPLE_FLOAT32LE));
  scm_c_define_gsubr("make-pulsedev"         , 6, 0, 0, SCM_FUNC(make_pulsedev   ));
  scm_c_define_gsubr("pulsedev-destroy"      , 1, 0, 0, SCM_FUNC(pulsedev_destroy));
  scm_c_define_gsubr("pulsedev-write"        , 3, 0, 0, SCM_FUNC(pulsedev_write  ));
  scm_c_define_gsubr("pulsedev-flush"        , 1, 0, 0, SCM_FUNC(pulsedev_flush  ));
  scm_c_define_gsubr("pulsedev-drain"        , 1, 0, 0, SCM_FUNC(pulsedev_drain  ));
  scm_c_define_gsubr("pulsedev-latency"      , 1, 0, 0, SCM_FUNC(pulsedev_latency));
  scm_c_define_gsubr("pulsedev-read"         , 2, 0, 0, SCM_FUNC(pulsedev_read   ));
}
