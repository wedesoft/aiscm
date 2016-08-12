#include <pthread.h>
#include <pulse/pulseaudio.h>
#include <libguile.h>
#include "ringbuffer.h"


// https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/
// http://freedesktop.org/software/pulseaudio/doxygen/pacat-simple_8c-example.html
// https://jan.newmarch.name/LinuxSound/Sampled/PulseAudio/

static scm_t_bits pulsedev_tag;

struct pulsedev_t {
  pa_sample_spec sample_spec;
  pthread_mutex_t mutex;
  char mutex_initialised;
  pthread_cond_t condition;
  char condition_initialised;
  struct ringbuffer_t ringbuffer;
  pa_mainloop *mainloop;
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
    pa_mainloop_free(self->mainloop);
    self->mainloop = NULL;
    self->mainloop_api = NULL;
  };
  if (self->ringbuffer.buffer) {
    ringbuffer_destroy(&self->ringbuffer);
    self->ringbuffer.buffer = NULL;
  };
  if (self->condition_initialised) {
    pthread_cond_destroy(&self->condition);
    self->condition_initialised = 0;
  };
  if (self->mutex_initialised) {
    pthread_mutex_destroy(&self->mutex);
    self->mutex_initialised = 0;
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
  //pa_usec_t usec;
  //int neg;
  //if (!pa_stream_get_latency(self->stream, &usec, &neg))
  //  printf("latency %s%8d us\n", neg ? "-" : "", (int)usec);
  pthread_mutex_lock(&self->mutex);
  if (self->ringbuffer.fill)
    ringbuffer_fetch(&self->ringbuffer, length, write_from_ringbuffer, self->stream);
  else
    pthread_cond_signal(&self->condition);
  pthread_mutex_unlock(&self->mutex);
}

void context_state_callback(pa_context *context, void *userdata)
{
  *(pa_context_state_t *)userdata = pa_context_get_state(context);
}

SCM make_pulsedev(SCM scm_name, SCM scm_type, SCM scm_channels, SCM scm_rate, SCM scm_latency)
{
  SCM retval;
  struct pulsedev_t *self = (struct pulsedev_t *)scm_gc_calloc(sizeof(struct pulsedev_t), "pulsedev");
  SCM_NEWSMOB(retval, pulsedev_tag, self);

  // TODO: split up into methods
  // initialise mutex and condition variable
  pthread_mutex_init(&self->mutex, NULL);
  self->mutex_initialised = 1;
  pthread_cond_init(&self->condition, NULL);
  self->condition_initialised = 1;

  // initialise ring buffer
  ringbuffer_init(&self->ringbuffer, 1024);

  // initialise main loop
  self->mainloop = pa_mainloop_new();
  self->mainloop_api = pa_mainloop_get_api(self->mainloop);

  // initialise context object
  self->context = pa_context_new(self->mainloop_api, "aiscm");
  pa_context_connect(self->context, NULL, 0, NULL);
  pa_context_state_t context_state = PA_CONTEXT_UNCONNECTED;
  pa_context_set_state_callback(self->context, context_state_callback, &context_state);
  while (context_state != PA_CONTEXT_READY)
    pa_mainloop_iterate(self->mainloop, 0, NULL);

  // initialise audio stream
  self->sample_spec.format = scm_to_int(scm_type);
  self->sample_spec.rate = scm_to_int(scm_rate);
  self->sample_spec.channels = scm_to_int(scm_channels);
  self->stream = pa_stream_new(self->context, "playback", &self->sample_spec, NULL);
  if (!self->stream)
    scm_misc_error("make-pulsedev", "Error creating audio stream: ~a",
                   scm_list_1(scm_from_locale_string(pa_strerror(pa_context_errno(self->context)))));
  pa_stream_set_write_callback(self->stream, stream_write_callback, self);

  // configure and connect audio stream
  static pa_stream_flags_t flags = PA_STREAM_INTERPOLATE_TIMING | PA_STREAM_AUTO_TIMING_UPDATE;
  pa_buffer_attr buffer_attr;
  buffer_attr.fragsize = (uint32_t)-1;
  int latency = (int)(scm_to_double(scm_latency) * 1e6);
  buffer_attr.maxlength = pa_usec_to_bytes(latency, &self->sample_spec);
  buffer_attr.minreq = pa_usec_to_bytes(0, &self->sample_spec);
  buffer_attr.prebuf = 0;
  buffer_attr.tlength = pa_usec_to_bytes(latency, &self->sample_spec);
  const char *name = scm_is_string(scm_name) ? scm_to_locale_string(scm_name) : NULL;
  pa_stream_connect_playback(self->stream, name, &buffer_attr, flags, NULL, NULL);

  return retval;
}

SCM pulsedev_mainloop_run(SCM scm_self)
{
  int retval;
  int error = pa_mainloop_run(get_self(scm_self)->mainloop, &retval);
  if (error < 0)
    scm_misc_error("pulsedev-mainloop-run", "Error running main loop", SCM_EOL);
  return scm_from_int(retval);
}

SCM pulsedev_mainloop_quit(SCM scm_self, SCM scm_result)
{
  pa_mainloop_quit(get_self(scm_self)->mainloop, scm_to_int(scm_result));
  return SCM_UNDEFINED;
}

SCM pulsedev_write(SCM scm_self, SCM scm_data, SCM scm_bytes)// TODO: check audio device still open
{
  struct pulsedev_t *self = get_self(scm_self);
  pthread_mutex_lock(&self->mutex);
  ringbuffer_store(&self->ringbuffer, scm_to_pointer(scm_data), scm_to_int(scm_bytes));
  pthread_mutex_unlock(&self->mutex);
  return SCM_UNSPECIFIED;
}

SCM pulsedev_flush(SCM scm_self)// TODO: check audio device still open
{
  struct pulsedev_t *self = get_self(scm_self);
  pthread_mutex_lock(&self->mutex);
  ringbuffer_flush(&self->ringbuffer);
  pa_stream_flush(self->stream, NULL, NULL);
  pthread_mutex_unlock(&self->mutex);
  return SCM_UNSPECIFIED;
}

SCM pulsedev_drain(SCM scm_self)// TODO: check audio device still open
{
  struct pulsedev_t *self = get_self(scm_self);
  pthread_mutex_lock(&self->mutex);
  if (self->ringbuffer.fill)
    pthread_cond_wait(&self->condition, &self->mutex);
  pthread_mutex_unlock(&self->mutex);
  pa_stream_drain(self->stream, NULL, NULL);
  return SCM_UNSPECIFIED;
}

SCM pulsedev_latency(SCM scm_self)// TODO: check audio device still open
{
  struct pulsedev_t *self = get_self(scm_self);
  pthread_mutex_lock(&self->mutex);
  pa_usec_t ringbuffer_usec = pa_bytes_to_usec(self->ringbuffer.fill, &self->sample_spec);
  pa_usec_t pulse_usec;
  int negative;
  pa_stream_get_latency(self->stream, &pulse_usec, &negative);
  double retval = (ringbuffer_usec + pulse_usec) * 1e-6;
  pthread_mutex_unlock(&self->mutex);
  return scm_from_double(retval);
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
  scm_c_define_gsubr("pulsedev-mainloop-run" , 1, 0, 0, pulsedev_mainloop_run );
  scm_c_define_gsubr("pulsedev-mainloop-quit", 2, 0, 0, pulsedev_mainloop_quit);
  scm_c_define_gsubr("pulsedev-write"        , 3, 0, 0, pulsedev_write        );
  scm_c_define_gsubr("pulsedev-flush"        , 1, 0, 0, pulsedev_flush        );
  scm_c_define_gsubr("pulsedev-drain"        , 1, 0, 0, pulsedev_drain        );
  scm_c_define_gsubr("pulsedev-latency"      , 1, 0, 0, pulsedev_latency      );
}
