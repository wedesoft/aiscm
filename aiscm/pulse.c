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
};

static struct pulsedev_t *get_self(SCM scm_self)
{
  scm_assert_smob_type(pulsedev_tag, scm_self);
  return (struct pulsedev_t *)SCM_SMOB_DATA(scm_self);
}

SCM pulsedev_destroy(SCM scm_self)
{
  struct pulsedev_t *self = get_self(scm_self);
  self->mainloop_api = NULL;
  if (self->context) {
    pa_context_disconnect(self->context);
    pa_context_unref(self->context);
    self->context = NULL;
  };
  if (self->mainloop) {
    pa_mainloop_free(self->mainloop);
    self->mainloop = NULL;
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

// static void pulse_error(const char *context, const char *format, int error)
// {
//    scm_misc_error(context, format, scm_list_1(scm_from_locale_string(pa_strerror(error))));
// }

SCM make_pulsedev(void)
{
  SCM retval;
  struct pulsedev_t *self = (struct pulsedev_t *)scm_gc_calloc(sizeof(struct pulsedev_t), "pulsedev");
  SCM_NEWSMOB(retval, pulsedev_tag, self);
  self->mainloop = pa_mainloop_new();
  self->mainloop_api = pa_mainloop_get_api(self->mainloop);
  self->context = pa_context_new(self->mainloop_api, "aiscm");
  pa_context_connect(self->context, NULL, 0, NULL);
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
  scm_c_define_gsubr("pulsedev-mainloop-run" , 1, 0, 0, pulsedev_mainloop_run );
  scm_c_define_gsubr("pulsedev-mainloop-quit", 2, 0, 0, pulsedev_mainloop_quit);
}
