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

int *from_int_vector(const std::vector<int> &vec)
{
  int *result = (int *)scm_gc_malloc_pointerless(vec.size() * sizeof(int), "int-vector");
  for (int i=0; i<vec.size(); i++)
    result[i] = vec[i];
  return result;
}

std::vector<int> to_int_vector(void *mem, int count)
{
  std::vector<int> result;
  for (int i=0; i<count; i++)
    result.push_back(((int *)mem)[i]);
  return result;
}

extern "C" {
  SCM opencv_connected_components(SCM scm_img, SCM scm_result, SCM scm_shape, SCM scm_connectivity, SCM scm_label_type)
  {
    int count;
    try {
      int height = scm_to_int(scm_car(scm_shape));
      int width = scm_to_int(scm_cadr(scm_shape));
      cv::Mat img(height, width, CV_8UC1, scm_to_pointer(scm_img));
      cv::Mat result(height, width, scm_to_int(scm_label_type), scm_to_pointer(scm_result));
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
      int marker_size = scm_to_int(scm_marker_size);
      int width = cols * size;
      int height = rows * size;
      cv::Mat result(height, width, CV_8UC1, scm_to_pointer(scm_result));
      cv::Ptr<cv::aruco::Dictionary> dict(cv::aruco::getPredefinedDictionary(scm_to_int(scm_dict)));
      cv::Ptr<cv::aruco::CharucoBoard> board(cv::aruco::CharucoBoard::create(cols, rows, size, marker_size, dict));
      board->draw(cv::Size(width, height), result, 0, 1);
    } catch (cv::Exception &e) {
      scm_misc_error("opencv-charuco-board", e.what(), SCM_EOL);
    }
    return SCM_UNDEFINED;
  }

  SCM opencv_detect_markers(SCM scm_shape, SCM scm_type, SCM scm_memory, SCM scm_dict)
  {
    int height = scm_to_int(scm_car(scm_shape));
    int width = scm_to_int(scm_cadr(scm_shape));
    cv::Mat img(height, width, scm_to_int(scm_type), scm_to_pointer(scm_memory));
    cv::Ptr<cv::aruco::Dictionary> dict(cv::aruco::getPredefinedDictionary(scm_to_int(scm_dict)));
    std::vector<int> marker_ids;
    std::vector<std::vector<cv::Point2f>> marker_corners;
    cv::aruco::detectMarkers(img, dict, marker_corners, marker_ids);
    int *ids = from_int_vector(marker_ids);
    float *markers = (float *)scm_gc_malloc_pointerless(marker_corners.size() * 8 * sizeof(float), "marker-corners");
    for (int j=0; j<marker_corners.size(); j++)
      for (int i=0; i<4; i++) {
        markers[j * 8 + 2 * i    ] = marker_corners[j][i].x;
        markers[j * 8 + 2 * i + 1] = marker_corners[j][i].y;
      };
    return scm_list_3(scm_from_int(marker_ids.size()),
                      scm_from_pointer(ids, NULL),
                      scm_from_pointer(markers, NULL));
  }

  SCM opencv_interpolate_corners(SCM scm_count, SCM scm_ids, SCM scm_markers, SCM scm_shape, SCM scm_image,
                                 SCM scm_rows, SCM scm_cols, SCM scm_size, SCM scm_marker_size)
  {
    int height = scm_to_int(scm_car(scm_shape));
    int width = scm_to_int(scm_cadr(scm_shape));
    cv::Mat img(height, width, CV_8UC1, scm_to_pointer(scm_image));
    int count = scm_to_int(scm_count);
    std::vector<int> marker_ids(to_int_vector(scm_to_pointer(scm_ids), count));
    std::vector<std::vector<cv::Point2f>> marker_corners;
    float *markers = (float *)scm_to_pointer(scm_markers);
    for (int j=0; j<count; j++) {
      std::vector<cv::Point2f> empty;
      marker_corners.push_back(empty);
      for (int i=0; i<4; i++) {
        cv::Point2f point(markers[j * 8 + 2 * i], markers[j * 8 + 2 * i + 1]);
        marker_corners[j].push_back(point);
      };
    };
    cv::Ptr<cv::aruco::Dictionary> dummy(cv::aruco::getPredefinedDictionary(cv::aruco::DICT_ARUCO_ORIGINAL));
    int rows = scm_to_int(scm_rows);
    int cols = scm_to_int(scm_cols);
    int size = scm_to_int(scm_size);
    int marker_size = scm_to_int(scm_marker_size);
    cv::Ptr<cv::aruco::CharucoBoard> board(cv::aruco::CharucoBoard::create(cols, rows, size, scm_to_int(scm_marker_size), dummy));
    std::vector<cv::Point2f> charuco_corners;
    std::vector<int> charuco_ids;
    cv::aruco::interpolateCornersCharuco(marker_corners, marker_ids, img, board, charuco_corners, charuco_ids);
    int *ids = from_int_vector(marker_ids);
    markers = (float *)scm_gc_malloc_pointerless(charuco_corners.size() * 2 * sizeof(float), "marker-corners");
    for (int j=0; j<charuco_corners.size(); j++) {
      markers[j * 2    ] = charuco_corners[j].x;
      markers[j * 2 + 1] = charuco_corners[j].y;
    };
    return scm_list_3(scm_from_int(charuco_ids.size()),
                      scm_from_pointer(ids, NULL),
                      scm_from_pointer(markers, NULL));
  }

  void init_opencv(void) {
    scm_c_define("CV_8UC1" , scm_from_int(CV_8UC1 ));
    scm_c_define("CV_8UC3" , scm_from_int(CV_8UC3 ));
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
    scm_c_define_gsubr("opencv-detect-markers"      , 4, 0, 0, SCM_FUNC(opencv_detect_markers      ));
    scm_c_define_gsubr("opencv-interpolate-corners" , 9, 0, 0, SCM_FUNC(opencv_interpolate_corners ));
  };
}
