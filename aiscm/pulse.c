#include <pulse/error.h>
#include <pulse/simple.h>
#include <libguile.h>

static scm_t_bits pulse_tag;

struct pulse_t {
  pa_simple *s;
};

SCM pulse_destroy(SCM scm_self)
{
  struct pulse_t *self = (struct pulse_t *)SCM_SMOB_DATA(scm_self);
  if (self->s) {
    pa_simple_free(self->s);
    self->s = NULL;
  };
  return SCM_UNSPECIFIED;
}

size_t free_pulse(SCM scm_self)
{
  struct pulse_t *self = (struct pulse_t *)SCM_SMOB_DATA(scm_self);
  pulse_destroy(scm_self);
  scm_gc_free(self, sizeof(struct pulse_t), "pulse");
  return 0;
}

SCM make_pulse(void)
{
  SCM retval;
  static const pa_sample_spec format = {
      .format = PA_SAMPLE_S16LE,
      .rate = 44100,
      .channels = 2
  };
  int error;
  pa_simple *s = pa_simple_new(NULL, "aiscm", PA_STREAM_PLAYBACK, NULL, "stream", &format, NULL, NULL, &error);
  if (!s) {
    SCM scm_str = scm_from_locale_string(pa_strerror(error));
    scm_misc_error("make_pulse", "Error initialising Pulse audio: ~a", scm_list_1(scm_str));
  };
  struct pulse_t *self = (struct pulse_t *)scm_gc_calloc(sizeof(struct pulse_t), "pulse");
  SCM_NEWSMOB(retval, pulse_tag, self);
  return retval;
}

void init_pulse(void)
{
  pulse_tag = scm_make_smob_type("pulse", sizeof(struct pulse_t));
  scm_set_smob_free(pulse_tag, free_pulse);
  scm_c_define_gsubr("make-pulse", 0, 0, 0, make_pulse);
  scm_c_define_gsubr("pulse-destroy", 1, 0, 0, pulse_destroy);
}
