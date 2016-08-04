#include <pulse/pulseaudio.h>
#include <libguile.h>

// https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/
// http://freedesktop.org/software/pulseaudio/doxygen/pacat-simple_8c-example.html
// https://jan.newmarch.name/LinuxSound/Sampled/PulseAudio/

static scm_t_bits pulsedev_tag;

struct pulsedev_t {
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
  return SCM_UNSPECIFIED;
}

size_t free_pulsedev(SCM scm_self)
{
  struct pulsedev_t *self = get_self(scm_self);
  pulsedev_destroy(scm_self);
  scm_gc_free(self, sizeof(struct pulsedev_t), "pulse");
  return 0;
}

static pa_sample_spec sample_spec = {// TODO: put into object
  .format = PA_SAMPLE_S16LE,
  .rate = 44100,
  .channels = 2
};

static char *odevice = NULL;// TODO: make string parameter

static void stream_write_callback(pa_stream *s, size_t length, void *userdata) {
  //struct pulsedev_t *self = (struct pulsedev_t *)userdata;
  //pa_usec_t usec;
  //int negative;
  //pa_stream_get_latency(s, &usec, &negative); // TODO: check error
}

void context_state_callback(pa_context *context, void *userdata)
{
  pa_context_state_t state = pa_context_get_state(context);
  struct pulsedev_t *self = (struct pulsedev_t *)userdata;
  if (state == PA_CONTEXT_READY) {
    self->stream = pa_stream_new(context, "playback", &sample_spec, NULL);// TODO: check error
    pa_stream_set_write_callback(self->stream, stream_write_callback, self);
    static pa_stream_flags_t flags = 0;
    pa_buffer_attr buffer_attr;
    memset(&buffer_attr, 0, sizeof(buffer_attr));
    pa_stream_connect_playback(self->stream, odevice, &buffer_attr, flags, NULL, NULL);// TODO: check error
  };
}

SCM make_pulsedev(void)
{
  SCM retval;
  struct pulsedev_t *self = (struct pulsedev_t *)scm_gc_calloc(sizeof(struct pulsedev_t), "pulsedev");
  SCM_NEWSMOB(retval, pulsedev_tag, self);
  self->mainloop = pa_mainloop_new();
  self->mainloop_api = pa_mainloop_get_api(self->mainloop);
  self->context = pa_context_new(self->mainloop_api, "aiscm");
  pa_context_connect(self->context, NULL, 0, NULL);
  pa_context_set_state_callback(self->context, context_state_callback, self);
  while (!self->stream)// TODO: handle errors
    pa_mainloop_iterate(self->mainloop, 0, NULL);
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

void init_pulse(void)
{
  pulsedev_tag = scm_make_smob_type("pulsedev", sizeof(struct pulsedev_t));
  scm_set_smob_free(pulsedev_tag, free_pulsedev);
  scm_c_define("PA_SAMPLE_U8"       , scm_from_int(PA_SAMPLE_U8       ));
  scm_c_define("PA_SAMPLE_S16LE"    , scm_from_int(PA_SAMPLE_S16LE    ));
  scm_c_define("PA_SAMPLE_S32LE"    , scm_from_int(PA_SAMPLE_S32LE    ));
  scm_c_define("PA_SAMPLE_FLOAT32LE", scm_from_int(PA_SAMPLE_FLOAT32LE));
  scm_c_define_gsubr("make-pulsedev"         , 0, 0, 0, make_pulsedev         );
  scm_c_define_gsubr("pulsedev-destroy"      , 1, 0, 0, pulsedev_destroy      );
  scm_c_define_gsubr("pulsedev-mainloop-run" , 1, 0, 0, pulsedev_mainloop_run );
  scm_c_define_gsubr("pulsedev-mainloop-quit", 2, 0, 0, pulsedev_mainloop_quit);
}
