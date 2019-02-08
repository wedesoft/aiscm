// AIscm - Guile extension for numerical arrays and tensors.
// Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018, 2019 Jan Wedekind <jan@wedesoft.de>
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
#include <libguile.h>
#include <opencv2/core.hpp>
#include <opencv2/imgproc.hpp>
#include "util-helpers.h"


extern "C" {
  SCM opencv_connected_components(SCM scm_img, SCM scm_result, SCM scm_shape, SCM scm_connectivity, SCM scm_label_type)
  {
    int count;
    try {
      int width = scm_to_int(scm_car(scm_shape));
      int height = scm_to_int(scm_cadr(scm_shape));
      cv::Mat img(height, width, CV_8UC1, scm_to_pointer(scm_img));
      cv::Mat result(height, width, scm_to_int(scm_label_type), scm_to_pointer(scm_result));
      count = connectedComponents(img, result, scm_to_int(scm_connectivity), scm_to_int(scm_label_type));
    } catch (cv::Exception &e) {
      scm_misc_error("opencv-connected-components", e.what(), SCM_EOL);
    }
    return scm_from_int(count);
  }

  void init_opencv(void) {
    scm_c_define("CV_8UC1" , scm_from_int(CV_8UC1 ));
    scm_c_define("CV_8SC1" , scm_from_int(CV_8SC1 ));
    scm_c_define("CV_16UC1", scm_from_int(CV_16UC1));
    scm_c_define("CV_16SC1", scm_from_int(CV_16SC1));
    scm_c_define("CV_32SC1", scm_from_int(CV_32SC1));
    scm_c_define_gsubr("opencv-connected-components", 5, 0, 0, SCM_FUNC(opencv_connected_components));
  };
}
