#include <pulse/error.h>
#include <pulse/simple.h>
#include <libguile.h>

// https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/
// http://freedesktop.org/software/pulseaudio/doxygen/pacat-simple_8c-example.html
// https://jan.newmarch.name/LinuxSound/Sampled/PulseAudio/

static scm_t_bits pulsedev_tag;

struct pulsedev_t {
  pa_simple *s;
};

static void device_not_open(const char *context)
{
  scm_misc_error(context, "Device is not open. Did you call 'destroy' before?", SCM_EOL);
}

static struct pulsedev_t *get_self(SCM scm_self)
{
  scm_assert_smob_type(pulsedev_tag, scm_self);
  return (struct pulsedev_t *)SCM_SMOB_DATA(scm_self);
}

SCM pulsedev_destroy(SCM scm_self)
{
  struct pulsedev_t *self = get_self(scm_self);
  if (self->s) {
    pa_simple_free(self->s);
    self->s = NULL;
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

static void pulse_error(const char *context, const char *format, int error)
{
   scm_misc_error(context, format, scm_list_1(scm_from_locale_string(pa_strerror(error))));
}

SCM make_pulsedev(SCM scm_direction, SCM scm_type, SCM scm_rate, SCM scm_channels)
{
  SCM retval;
  pa_sample_spec sample_spec;
  sample_spec.format = scm_to_int(scm_type);
  sample_spec.rate = scm_to_int(scm_rate);
  sample_spec.channels = scm_to_int(scm_channels);
  struct pulsedev_t *self = (struct pulsedev_t *)scm_gc_calloc(sizeof(struct pulsedev_t), "pulsedev");
  SCM_NEWSMOB(retval, pulsedev_tag, self);
  int error;
  self->s = pa_simple_new(NULL, "aiscm", scm_to_int(scm_direction), NULL, "stream", &sample_spec, NULL, NULL, &error);
  if (!self->s)
    scm_misc_error("make-pulsedev", "Error initialising Pulse audio: ~a",
                   scm_list_1(scm_from_locale_string(pa_strerror(error))));
  return retval;
}

SCM pulsedev_write(SCM scm_self, SCM scm_data, SCM scm_bytes)
{
  struct pulsedev_t *self = get_self(scm_self);
  if (!self->s) device_not_open("pulsedev-write");
  int error = PA_OK;
  if (pa_simple_write(self->s, scm_to_pointer(scm_data), scm_to_int(scm_bytes), &error) < 0)
    pulse_error("pulsedev-write", "Error writing audio samples: ~a", error);
  return SCM_UNSPECIFIED;
}

SCM pulsedev_read(SCM scm_self, SCM scm_data, SCM scm_bytes)
{
  struct pulsedev_t *self = get_self(scm_self);
  if (!self->s) device_not_open("pulsedev-read");
  int error = PA_OK;
  if (pa_simple_read(self->s, scm_to_pointer(scm_data), scm_to_int(scm_bytes), &error) < 0)
    pulse_error("pulsedev-read", "Error reading audio samples: ~a", error);
  return SCM_UNSPECIFIED;
}

SCM pulsedev_latency(SCM scm_self)
{
  struct pulsedev_t *self = get_self(scm_self);
  if (!self->s) device_not_open("pulsedev-latency");
  int error = PA_OK;
  pa_usec_t latency = pa_simple_get_latency(self->s, &error);
  if (error != PA_OK)
    pulse_error("pulsedev-latency", "Error getting latency: ~a", error);
  return scm_from_int(latency);
}

SCM pulsedev_drain(SCM scm_self)
{
  struct pulsedev_t *self = get_self(scm_self);
  if (!self->s) device_not_open("pulsedev-drain");
  int error = PA_OK;
  pa_simple_drain(self->s, &error);
  if (error != PA_OK)
    pulse_error("pulsedev-drain", "Error waiting for data to be written: ~a", error);
  return SCM_UNSPECIFIED;
}

SCM pulsedev_flush(SCM scm_self)
{
  struct pulsedev_t *self = get_self(scm_self);
  if (!self->s) device_not_open("pulsedev-flush");
  int error = PA_OK;
  pa_simple_flush(self->s, &error);
  if (error != PA_OK)
    pulse_error("pulsedev-flush", "Error discarding buffered audio samples: ~a", error);
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
  scm_c_define("PA_STREAM_PLAYBACK", scm_from_int(PA_STREAM_PLAYBACK));
  scm_c_define("PA_STREAM_RECORD", scm_from_int(PA_STREAM_RECORD));
  scm_c_define_gsubr("make-pulsedev"   , 4, 0, 0, make_pulsedev);
  scm_c_define_gsubr("pulsedev-destroy", 1, 0, 0, pulsedev_destroy);
  scm_c_define_gsubr("pulsedev-write"  , 3, 0, 0, pulsedev_write);
  scm_c_define_gsubr("pulsedev-read"   , 3, 0, 0, pulsedev_read);
  scm_c_define_gsubr("pulsedev-latency", 1, 0, 0, pulsedev_latency);
  scm_c_define_gsubr("pulsedev-drain"  , 1, 0, 0, pulsedev_drain);
  scm_c_define_gsubr("pulsedev-flush"  , 1, 0, 0, pulsedev_flush);
}
