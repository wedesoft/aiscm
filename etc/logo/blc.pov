#include "colors.inc"
#include "textures.inc"
#include "metals.inc"
#include "glass.inc"

global_settings {
  charset utf8
  assumed_gamma 2.2
  max_trace_level 5
}

camera {
  location  < -6,  3.2, -2 >
  look_at   <  0,  0,  0 >
  up y
  right x
  angle 17
}

background {
   color rgbt< 0.4, 0.4, 0.3, 1 >
}

light_source {
  <-330, 650,-1000>
  rgb < 1, 1, 1 >
}

sky_sphere {
  pigment {
     Clouds
  }
  scale < 0.5, 0.1, 0.5 >
}

union {
  difference {
    box {
      < -0.5, -0.5, -0.5 >,
      <  0.5,  0.5,  0.5 >
    }
    plane { < -1,  1, 0 >, -0.35 }
    difference {
      plane { <  1, -1,  0 >, -0.2 }
      plane { < -1,  0,  0 >, -0.2 }
      plane { <  0,  1,  0 >, -0.2 }
      plane { <  0,  0,  1 >, -0.4 }
      plane { <  0,  0, -1 >, -0.4 }
    }
    plane { <  1, -1,  0 >, -0.4 }
    union {
      union {
        cylinder {
          <  0.35,  1   , -0.3 >,
          <  0.35,  0.11, -0.3 >,
          0.12
        }
        cylinder {
          <  0.35,  1   ,  0   >,
          <  0.35,  0.11,  0   >,
          0.12
        }
        cylinder {
          <  0.35,  1   ,  0.3 >,
          <  0.35,  0.11,  0.3 >,
          0.12
        }
      }
      union {
        cylinder {
          < -1   , -0.35, -0.3 >,
          < -0.11, -0.35, -0.3 >,
          0.12
        }
        cylinder {
          < -1   , -0.35,  0   >,
          < -0.11, -0.35,  0   >,
          0.12
        }
        cylinder {
          < -1   , -0.35,  0.3 >,
          < -0.11, -0.35,  0.3 >,
          0.12
        }
      }
      texture {
        pigment { color rgb < 0.2, 0.2, 1.0 > }
        finish { F_MetalD }
      }
    }
    union {
      cylinder {
        <  0.5,  0.5, -0.5 >,
        < -0.2, -0.2, -0.5 >,
        0.03
      }
      cylinder {
        <  0.5,  0.5,  0.5 >,
        < -0.2, -0.2,  0.5 >,
        0.03
      }
      cylinder {
        <  0.3,  0.1, -0.5 >,
        < -0.4, -0.6, -0.5 >,
        0.03
      }
      cylinder {
        <  0.3,  0.1,  0.5 >,
        < -0.4, -0.6,  0.5 >,
        0.03
      }
    }
    texture { T_Silver_5E }
  }
  cylinder {
    < 0.35, -0.35, -0.55 >,
    < 0.35, -0.35,  0.55 >,
    0.35
    texture { T_Silver_5E }
  }
  text {
    ttf "opensans.ttf" "λ" 0.18, 0
    scale < 2, 2, 1 >
    translate < -0.80, -0.9, -0.90 >
    rotate x * 45
    rotate y * 90
    scale 0.3
    pigment { Col_Glass_Ruby }
    finish { F_Glass10 }
    interior { I_Glass4 }
  }
  rotate (z + y) * (20.0 + 360.0 * clock)
}
