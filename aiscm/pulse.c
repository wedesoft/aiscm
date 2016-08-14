#include <pthread.h>
#include <pulse/pulseaudio.h>
#include <unistd.h>
#include <libguile.h>
#include "ringbuffer.h"


// https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/
// http://freedesktop.org/software/pulseaudio/doxygen/pacat-simple_8c-example.html
// https://jan.newmarch.name/LinuxSound/Sampled/PulseAudio/

static scm_t_bits pulsedev_tag;

struct pulsedev_t {
  pa_sample_spec sample_spec;
  struct ringbuffer_t ringbuffer;
  pa_threaded_mainloop *mainloop;
  pa_mainloop_api *mainloop_api;
  pa_context *context;
  pa_stream *stream;
};

static struct pulsedev_t *get_self(SCM scm_self)
{
  scm_assert_smob_type(pulsedev_tag, scm_self);
  return (struct pulsedev_t *)SCM_SMOB_DATA(scm_self);
}

SCM pulsedev_destroy(SCM scm_self)
{
  struct pulsedev_t *self = get_self(scm_self);
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
  if (self->ringbuffer.buffer) {
    ringbuffer_destroy(&self->ringbuffer);
    self->ringbuffer.buffer = NULL;
  };
  return SCM_UNSPECIFIED;
}

size_t free_pulsedev(SCM scm_self)
{
  struct pulsedev_t *self = get_self(scm_self);
  pulsedev_destroy(scm_self);
  scm_gc_free(self, sizeof(struct pulsedev_t), "pulse");
  return 0;
}

static void write_from_ringbuffer(char *data, int count, void *userdata)
{
  pa_stream_write((pa_stream *)userdata, data, count, NULL, 0LL, PA_SEEK_RELATIVE);
}

static void stream_write_callback(pa_stream *s, size_t length, void *userdata) {
  struct pulsedev_t *self = (struct pulsedev_t *)userdata;
  if (self->ringbuffer.fill)
    ringbuffer_fetch(&self->ringbuffer, length, write_from_ringbuffer, self->stream);
  else
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
  pa_threaded_mainloop_start(self->mainloop);// TODO: check for error
  while (context_state != PA_CONTEXT_READY)
    pa_threaded_mainloop_wait(self->mainloop);
}

static void initialise_stream(struct pulsedev_t *self)
{
  self->stream = pa_stream_new(self->context, "playback", &self->sample_spec, NULL);
  if (!self->stream)
    scm_misc_error("make-pulsedev", "Error creating audio stream: ~a",
                   scm_list_1(scm_from_locale_string(pa_strerror(pa_context_errno(self->context)))));
  pa_stream_set_write_callback(self->stream, stream_write_callback, self);
}

static void connect_stream(struct pulsedev_t *self, const char *name, pa_usec_t latency)
{
  static pa_stream_flags_t flags = PA_STREAM_INTERPOLATE_TIMING | PA_STREAM_AUTO_TIMING_UPDATE;
  pa_buffer_attr buffer_attr;
  buffer_attr.fragsize = (uint32_t)-1;
  buffer_attr.maxlength = pa_usec_to_bytes(latency, &self->sample_spec);
  buffer_attr.minreq = pa_usec_to_bytes(0, &self->sample_spec);
  buffer_attr.prebuf = 0;
  buffer_attr.tlength = pa_usec_to_bytes(latency, &self->sample_spec);
  pa_stream_connect_playback(self->stream, name, &buffer_attr, flags, NULL, NULL);
}

SCM make_pulsedev(SCM scm_name, SCM scm_type, SCM scm_channels, SCM scm_rate, SCM scm_latency)
{
  SCM retval;
  struct pulsedev_t *self = (struct pulsedev_t *)scm_gc_calloc(sizeof(struct pulsedev_t), "pulsedev");
  SCM_NEWSMOB(retval, pulsedev_tag, self);

  const char *name = scm_is_string(scm_name) ? scm_to_locale_string(scm_name) : NULL;
  pa_usec_t latency = (pa_usec_t)(scm_to_double(scm_latency) * 1e6);
  self->sample_spec.format = scm_to_int(scm_type);
  self->sample_spec.rate = scm_to_int(scm_rate);
  self->sample_spec.channels = scm_to_int(scm_channels);

  ringbuffer_init(&self->ringbuffer, 1024);
  initialise_mainloop(self);
  initialise_context(self);
  initialise_stream(self);
  connect_stream(self, name, latency);

  return retval;
}

SCM pulsedev_write(SCM scm_self, SCM scm_data, SCM scm_bytes)// TODO: check audio device still open
{
  struct pulsedev_t *self = get_self(scm_self);
  pa_threaded_mainloop_lock(self->mainloop);
  ringbuffer_store(&self->ringbuffer, scm_to_pointer(scm_data), scm_to_int(scm_bytes));
  pa_threaded_mainloop_unlock(self->mainloop);
  return SCM_UNSPECIFIED;
}

void wait_for_flush(pa_stream *stream, int success, void *userdata)
{
  pa_threaded_mainloop_signal(userdata, 0);
}

SCM pulsedev_flush(SCM scm_self)// TODO: check audio device still open
{
  struct pulsedev_t *self = get_self(scm_self);
  pa_threaded_mainloop_lock(self->mainloop);
  ringbuffer_flush(&self->ringbuffer);
  pa_operation *operation = pa_stream_flush(self->stream, wait_for_flush, self->mainloop);
  while (pa_operation_get_state(operation) == PA_OPERATION_RUNNING)
    pa_threaded_mainloop_wait(self->mainloop);
  pa_operation_unref(operation);
  pa_threaded_mainloop_unlock(self->mainloop);
  return SCM_UNSPECIFIED;
}

useconds_t latency_usec(struct pulsedev_t *self)
{
  pa_threaded_mainloop_lock(self->mainloop);
  pa_usec_t ringbuffer_usec = pa_bytes_to_usec(self->ringbuffer.fill, &self->sample_spec);
  pa_usec_t pulse_usec;
  int negative;
  pa_stream_get_latency(self->stream, &pulse_usec, &negative);
  useconds_t retval =  negative ? ringbuffer_usec - pulse_usec : ringbuffer_usec + pulse_usec;
  pa_threaded_mainloop_unlock(self->mainloop);
  return retval;
}

SCM pulsedev_drain(SCM scm_self)// TODO: check audio device still open
{
  struct pulsedev_t *self = get_self(scm_self);
  pa_threaded_mainloop_lock(self->mainloop);
  while (self->ringbuffer.fill)
    pa_threaded_mainloop_wait(self->mainloop);
  useconds_t usecs_remaining = latency_usec(self);
  pa_threaded_mainloop_unlock(self->mainloop);
  usleep(usecs_remaining);
  return SCM_UNSPECIFIED;
}

SCM pulsedev_latency(SCM scm_self)// TODO: check audio device still open
{
  struct pulsedev_t *self = get_self(scm_self);
  return scm_from_double(1e6 * latency_usec(self));
}

void init_pulse(void)
{
  pulsedev_tag = scm_make_smob_type("pulsedev", sizeof(struct pulsedev_t));
  scm_set_smob_free(pulsedev_tag, free_pulsedev);
  scm_c_define("PA_SAMPLE_U8"       , scm_from_int(PA_SAMPLE_U8       ));
  scm_c_define("PA_SAMPLE_S16LE"    , scm_from_int(PA_SAMPLE_S16LE    ));
  scm_c_define("PA_SAMPLE_S32LE"    , scm_from_int(PA_SAMPLE_S32LE    ));
  scm_c_define("PA_SAMPLE_FLOAT32LE", scm_from_int(PA_SAMPLE_FLOAT32LE));
  scm_c_define_gsubr("make-pulsedev"         , 5, 0, 0, make_pulsedev         );
  scm_c_define_gsubr("pulsedev-destroy"      , 1, 0, 0, pulsedev_destroy      );
  scm_c_define_gsubr("pulsedev-write"        , 3, 0, 0, pulsedev_write        );
  scm_c_define_gsubr("pulsedev-flush"        , 1, 0, 0, pulsedev_flush        );
  scm_c_define_gsubr("pulsedev-drain"        , 1, 0, 0, pulsedev_drain        );
  scm_c_define_gsubr("pulsedev-latency"      , 1, 0, 0, pulsedev_latency      );
}
