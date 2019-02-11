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
#include <opencv2/aruco/charuco.hpp>
#include <opencv2/imgproc.hpp>
#include "util-helpers.h"


extern "C" {
  SCM opencv_connected_components(SCM scm_img, SCM scm_result, SCM scm_shape, SCM scm_connectivity, SCM scm_label_type)
  {
    int count;
    try {
      int width = scm_to_int(scm_car(scm_shape));
      int height = scm_to_int(scm_cadr(scm_shape));
      cv::Mat img(width, height, CV_8UC1, scm_to_pointer(scm_img));
      cv::Mat result(width, height, scm_to_int(scm_label_type), scm_to_pointer(scm_result));
      count = connectedComponents(img, result, scm_to_int(scm_connectivity), scm_to_int(scm_label_type));
    } catch (cv::Exception &e) {
      scm_misc_error("opencv-connected-components", e.what(), SCM_EOL);
    }
    return scm_from_int(count);
  }

  SCM opencv_charuco_board(SCM scm_result, SCM scm_rows, SCM scm_cols, SCM scm_size, SCM scm_marker_size, SCM scm_dict)
  {
    try {
      int rows = scm_to_int(scm_rows);
      int cols = scm_to_int(scm_cols);
      int size = scm_to_int(scm_size);
      int width = cols * size;
      int height = rows * size;
      cv::Mat result(width, height, CV_8UC1);
      cv::Ptr<cv::aruco::Dictionary> dict(cv::aruco::getPredefinedDictionary(scm_to_int(scm_dict)));
      cv::Ptr<cv::aruco::CharucoBoard> board(cv::aruco::CharucoBoard::create(cols, rows, size, scm_to_int(scm_marker_size), dict));
      board->draw(cv::Size(width, height), result, 0, 1);
    } catch (cv::Exception &e) {
      scm_misc_error("opencv-charuco-board", e.what(), SCM_EOL);
    }
    return SCM_UNDEFINED;
  }

  void init_opencv(void) {
    scm_c_define("CV_8UC1" , scm_from_int(CV_8UC1 ));
    scm_c_define("CV_8SC1" , scm_from_int(CV_8SC1 ));
    scm_c_define("CV_16UC1", scm_from_int(CV_16UC1));
    scm_c_define("CV_16SC1", scm_from_int(CV_16SC1));
    scm_c_define("CV_32SC1", scm_from_int(CV_32SC1));
    scm_c_define("DICT_4X4_50"        , scm_from_int(cv::aruco::DICT_4X4_50        ));
    scm_c_define("DICT_4X4_50"        , scm_from_int(cv::aruco::DICT_4X4_50        ));
    scm_c_define("DICT_4X4_100"       , scm_from_int(cv::aruco::DICT_4X4_100       ));
    scm_c_define("DICT_4X4_250"       , scm_from_int(cv::aruco::DICT_4X4_250       ));
    scm_c_define("DICT_4X4_1000"      , scm_from_int(cv::aruco::DICT_4X4_1000      ));
    scm_c_define("DICT_5X5_50"        , scm_from_int(cv::aruco::DICT_5X5_50        ));
    scm_c_define("DICT_5X5_100"       , scm_from_int(cv::aruco::DICT_5X5_100       ));
    scm_c_define("DICT_5X5_250"       , scm_from_int(cv::aruco::DICT_5X5_250       ));
    scm_c_define("DICT_5X5_1000"      , scm_from_int(cv::aruco::DICT_5X5_1000      ));
    scm_c_define("DICT_6X6_50"        , scm_from_int(cv::aruco::DICT_6X6_50        ));
    scm_c_define("DICT_6X6_100"       , scm_from_int(cv::aruco::DICT_6X6_100       ));
    scm_c_define("DICT_6X6_250"       , scm_from_int(cv::aruco::DICT_6X6_250       ));
    scm_c_define("DICT_6X6_1000"      , scm_from_int(cv::aruco::DICT_6X6_1000      ));
    scm_c_define("DICT_7X7_50"        , scm_from_int(cv::aruco::DICT_7X7_50        ));
    scm_c_define("DICT_7X7_100"       , scm_from_int(cv::aruco::DICT_7X7_100       ));
    scm_c_define("DICT_7X7_250"       , scm_from_int(cv::aruco::DICT_7X7_250       ));
    scm_c_define("DICT_7X7_1000"      , scm_from_int(cv::aruco::DICT_7X7_1000      ));
    scm_c_define("DICT_ARUCO_ORIGINAL", scm_from_int(cv::aruco::DICT_ARUCO_ORIGINAL));
    scm_c_define("DICT_APRILTAG_16h5" , scm_from_int(cv::aruco::DICT_APRILTAG_16h5 ));
    scm_c_define("DICT_APRILTAG_25h9" , scm_from_int(cv::aruco::DICT_APRILTAG_25h9 ));
    scm_c_define("DICT_APRILTAG_36h10", scm_from_int(cv::aruco::DICT_APRILTAG_36h10));
    scm_c_define("DICT_APRILTAG_36h11", scm_from_int(cv::aruco::DICT_APRILTAG_36h11));

    scm_c_define_gsubr("opencv-connected-components", 5, 0, 0, SCM_FUNC(opencv_connected_components));
    scm_c_define_gsubr("opencv-charuco-board"       , 6, 0, 0, SCM_FUNC(opencv_charuco_board       ));
  };
}
