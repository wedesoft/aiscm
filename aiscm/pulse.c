#include <pulse/pulseaudio.h>
#include <libguile.h>

// https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/
// http://freedesktop.org/software/pulseaudio/doxygen/pacat-simple_8c-example.html
// https://jan.newmarch.name/LinuxSound/Sampled/PulseAudio/

void init_pulse(void)
{
  scm_c_define("PA_SAMPLE_U8"       , scm_from_int(PA_SAMPLE_U8       ));
  scm_c_define("PA_SAMPLE_S16LE"    , scm_from_int(PA_SAMPLE_S16LE    ));
  scm_c_define("PA_SAMPLE_S32LE"    , scm_from_int(PA_SAMPLE_S32LE    ));
  scm_c_define("PA_SAMPLE_FLOAT32LE", scm_from_int(PA_SAMPLE_FLOAT32LE));
}
