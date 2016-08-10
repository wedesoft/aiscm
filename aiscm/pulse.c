#include <pulse/pulseaudio.h>
#include <libguile.h>
#include "ringbuffer.h"

#include <math.h> // TODO: remove this

// https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/
// http://freedesktop.org/software/pulseaudio/doxygen/pacat-simple_8c-example.html
// https://jan.newmarch.name/LinuxSound/Sampled/PulseAudio/

static scm_t_bits pulsedev_tag;

struct pulsedev_t {
  pa_sample_spec sample_spec;
  pthread_mutex_t mutex;
  char mutex_initialised;
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
  //printf("%d\n", length);
  pthread_mutex_lock(&self->mutex);
  ringbuffer_fetch(&self->ringbuffer, length, write_from_ringbuffer, self->stream);
  pthread_mutex_unlock(&self->mutex);
}

void context_state_callback(pa_context *context, void *userdata)
{
  pa_context_state_t state = pa_context_get_state(context);
  if (state == PA_CONTEXT_READY) *(char *)userdata = 1;
}

SCM make_pulsedev(SCM scm_name, SCM scm_type, SCM scm_channels, SCM scm_rate, SCM scm_latency)
{
  SCM retval;
  struct pulsedev_t *self = (struct pulsedev_t *)scm_gc_calloc(sizeof(struct pulsedev_t), "pulsedev");
  SCM_NEWSMOB(retval, pulsedev_tag, self);
  self->sample_spec.format = scm_to_int(scm_type);
  self->sample_spec.rate = scm_to_int(scm_rate);
  self->sample_spec.channels = scm_to_int(scm_channels);
  pthread_mutex_init(&self->mutex, NULL);
  self->mutex_initialised = 1;
  ringbuffer_init(&self->ringbuffer, 1024);
  self->mainloop = pa_mainloop_new();
  self->mainloop_api = pa_mainloop_get_api(self->mainloop);
  self->context = pa_context_new(self->mainloop_api, "aiscm");
  pa_context_connect(self->context, NULL, 0, NULL);
  char context_ready = 0;
  pa_context_set_state_callback(self->context, context_state_callback, &context_ready);
  while (!context_ready)
    pa_mainloop_iterate(self->mainloop, 0, NULL);
  self->stream = pa_stream_new(self->context, "playback", &self->sample_spec, NULL);
  if (!self->stream)
    scm_misc_error("make-pulsedev", "Error creating audio stream: ~a",
                   scm_list_1(scm_from_locale_string(pa_strerror(pa_context_errno(self->context)))));
  pa_stream_set_write_callback(self->stream, stream_write_callback, self);
  static pa_stream_flags_t flags = PA_STREAM_INTERPOLATE_TIMING | PA_STREAM_ADJUST_LATENCY | PA_STREAM_AUTO_TIMING_UPDATE;
  pa_buffer_attr buffer_attr;
  buffer_attr.fragsize = (uint32_t)-1;
  int latency = (int)(scm_to_double(scm_latency) * 1e6);
  buffer_attr.maxlength = pa_usec_to_bytes(latency, &self->sample_spec);
  buffer_attr.minreq = pa_usec_to_bytes(0, &self->sample_spec);
  buffer_attr.prebuf = (uint32_t)-1;
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
}
