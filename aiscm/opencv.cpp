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
#include <opencv2/calib3d.hpp>
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

cv::Mat to_cvmat(SCM scm_shape, SCM scm_type, SCM scm_pointer)
{
  int height = scm_to_int(scm_car(scm_shape));
  int width = scm_to_int(scm_cadr(scm_shape));
  return cv::Mat(height, width, scm_to_int(scm_type), scm_to_pointer(scm_pointer));
}

float *from_point_vector(const std::vector<cv::Point2f> &vec)
{
  float *result = (float *)scm_gc_malloc_pointerless(vec.size() * 2 * sizeof(float), "corners");
  for (int j=0; j<vec.size(); j++) {
    result[j * 2    ] = vec[j].x;
    result[j * 2 + 1] = vec[j].y;
  };
  return result;
}

std::vector<cv::Point2f> to_point_vector(void *mem, int count)
{
  std::vector<cv::Point2f> result;
  for (int i=0; i<count; i++) {
    cv::Point2f point(((float *)mem)[2 * i], ((float *)mem)[2 * i + 1]);
    result.push_back(point);
  };
  return result;
}

float *from_point_vector_vector(const std::vector<std::vector<cv::Point2f>> &vec)
{
  float *result = (float *)scm_gc_malloc_pointerless(vec.size() * 8 * sizeof(float), "marker-corners");
  for (int j=0; j<vec.size(); j++)
    for (int i=0; i<4; i++) {
      result[j * 8 + 2 * i    ] = vec[j][i].x;
      result[j * 8 + 2 * i + 1] = vec[j][i].y;
    };
  return result;
}

std::vector<std::vector<cv::Point2f>> to_point_vector_vector(void *mem, int count)
{
  std::vector<std::vector<cv::Point2f>> result;
  for (int j=0; j<count; j++) {
    std::vector<cv::Point2f> empty;
    result.push_back(empty);
    for (int i=0; i<4; i++) {
      cv::Point2f point(((float *)mem)[j * 8 + 2 * i], ((float *)mem)[j * 8 + 2 * i + 1]);
      result[j].push_back(point);
    };
  };
  return result;
}

std::vector<std::vector<cv::Point3f>> to_point3f_vector_vector(SCM scm_count, SCM scm_sizes, SCM scm_points)
{
  int count = scm_to_int(scm_count);
  std::vector<std::vector<cv::Point3f>> result;
  for (int i=0; i<count; i++) {
    std::vector<cv::Point3f> points;
    float *ptr = (float *)scm_to_pointer(scm_car(scm_points));
    int size = scm_to_int(scm_car(scm_sizes));
    for (int j=0; j<size; j++) {
      cv::Point3f p(ptr[3 * j], ptr[3 * j + 1], ptr[3 * j + 2]);
      points.push_back(p);
    };
    result.push_back(points);
    scm_sizes = scm_cdr(scm_sizes);
    scm_points = scm_cdr(scm_points);
  };
  return result;
}

std::vector<std::vector<cv::Point2f>> to_point2f_vector_vector(SCM scm_count, SCM scm_sizes, SCM scm_points)
{
  int count = scm_to_int(scm_count);
  std::vector<std::vector<cv::Point2f>> result;
  for (int i=0; i<count; i++) {
    std::vector<cv::Point2f> points;
    float *ptr = (float *)scm_to_pointer(scm_car(scm_points));
    int size = scm_to_int(scm_car(scm_sizes));
    for (int j=0; j<size; j++) {
      cv::Point2f p(ptr[2 * j], ptr[2 * j + 1]);
      points.push_back(p);
    };
    result.push_back(points);
    scm_sizes = scm_cdr(scm_sizes);
    scm_points = scm_cdr(scm_points);
  };
  return result;
}

extern "C" {
  SCM opencv_connected_components(SCM scm_img, SCM scm_result, SCM scm_shape, SCM scm_type, SCM scm_connectivity, SCM scm_label_type)
  {
    int count;
    try {
      cv::Mat img(to_cvmat(scm_shape, scm_type, scm_img));
      cv::Mat result(to_cvmat(scm_shape, scm_label_type, scm_result));
      count = connectedComponents(img, result, scm_to_int(scm_connectivity), scm_to_int(scm_label_type));
    } catch (cv::Exception &e) {
      scm_misc_error("opencv-connected-components", e.what(), SCM_EOL);
    }
    return scm_from_int(count);
  }

  SCM opencv_charuco_board(SCM scm_result, SCM scm_shape, SCM scm_rows, SCM scm_cols, SCM scm_size, SCM scm_marker_size, SCM scm_dict)
  {
    try {
      int rows = scm_to_int(scm_rows);
      int cols = scm_to_int(scm_cols);
      int size = scm_to_int(scm_size);
      int marker_size = scm_to_int(scm_marker_size);
      cv::Mat result(to_cvmat(scm_shape, scm_from_int(CV_8UC1), scm_result));
      cv::Ptr<cv::aruco::Dictionary> dict(cv::aruco::getPredefinedDictionary(scm_to_int(scm_dict)));
      cv::Ptr<cv::aruco::CharucoBoard> board(cv::aruco::CharucoBoard::create(cols, rows, size, marker_size, dict));
      board->draw(cv::Size(scm_to_int(scm_cadr(scm_shape)), scm_to_int(scm_car(scm_shape))), result, 0, 1);
    } catch (cv::Exception &e) {
      scm_misc_error("opencv-charuco-board", e.what(), SCM_EOL);
    }
    return SCM_UNDEFINED;
  }

  SCM opencv_draw_marker(SCM scm_memory, SCM scm_size, SCM scm_id, SCM scm_dict)
  {
    try {
      cv::Mat result(to_cvmat(scm_list_2(scm_size, scm_size), scm_from_int(CV_8UC1), scm_memory));
      cv::Ptr<cv::aruco::Dictionary> dict(cv::aruco::getPredefinedDictionary(scm_to_int(scm_dict)));
      cv::aruco::drawMarker(dict, scm_to_int(scm_id), scm_to_int(scm_size), result);
    } catch (cv::Exception &e) {
      scm_misc_error("opencv-draw-marker", e.what(), SCM_EOL);
    }
    return SCM_UNDEFINED;
  }

  SCM opencv_detect_markers(SCM scm_shape, SCM scm_type, SCM scm_memory, SCM scm_dict)
  {
    cv::Mat img(to_cvmat(scm_shape, scm_type, scm_memory));
    cv::Ptr<cv::aruco::Dictionary> dict(cv::aruco::getPredefinedDictionary(scm_to_int(scm_dict)));
    std::vector<int> marker_ids;
    std::vector<std::vector<cv::Point2f>> marker_corners;
    try {
      cv::aruco::detectMarkers(img, dict, marker_corners, marker_ids);
    } catch (cv::Exception &e) {
      scm_misc_error("opencv-detect-markers", e.what(), SCM_EOL);
    }
    int *ids = from_int_vector(marker_ids);
    float *markers = from_point_vector_vector(marker_corners);
    return scm_list_3(scm_from_int(marker_ids.size()),
                      scm_from_pointer(ids, NULL),
                      scm_from_pointer(markers, NULL));
  }

  SCM opencv_interpolate_corners(SCM scm_count, SCM scm_ids, SCM scm_markers, SCM scm_shape, SCM scm_type, SCM scm_image,
                                 SCM scm_rows, SCM scm_cols, SCM scm_size, SCM scm_marker_size)
  {
    cv::Mat img(to_cvmat(scm_shape, scm_type, scm_image));
    int count = scm_to_int(scm_count);
    std::vector<int> marker_ids(to_int_vector(scm_to_pointer(scm_ids), count));
    std::vector<std::vector<cv::Point2f>> marker_corners(to_point_vector_vector(scm_to_pointer(scm_markers), count));
    int rows = scm_to_int(scm_rows);
    int cols = scm_to_int(scm_cols);
    int size = scm_to_int(scm_size);
    int marker_size = scm_to_int(scm_marker_size);
    std::vector<cv::Point2f> charuco_corners;
    std::vector<int> charuco_ids;
    try {
      cv::Ptr<cv::aruco::Dictionary> dummy(cv::aruco::getPredefinedDictionary(cv::aruco::DICT_ARUCO_ORIGINAL));
      cv::Ptr<cv::aruco::CharucoBoard> board(cv::aruco::CharucoBoard::create(cols, rows, size, scm_to_int(scm_marker_size), dummy));
      cv::aruco::interpolateCornersCharuco(marker_corners, marker_ids, img, board, charuco_corners, charuco_ids);
    } catch (cv::Exception &e) {
      scm_misc_error("opencv-charuco-board", e.what(), SCM_EOL);
    }
    int *ids = from_int_vector(charuco_ids);
    float *markers = from_point_vector(charuco_corners);
    return scm_list_3(scm_from_int(charuco_ids.size()),
                      scm_from_pointer(ids, NULL),
                      scm_from_pointer(markers, NULL));
  }

  SCM opencv_draw_corners(SCM scm_shape, SCM scm_type, SCM scm_image, SCM scm_count, SCM scm_ids, SCM scm_corners)
  {
    cv::Mat img(to_cvmat(scm_shape, scm_type, scm_image));
    int count = scm_to_int(scm_count);
    std::vector<int> charuco_ids(to_int_vector(scm_to_pointer(scm_ids), count));
    std::vector<cv::Point2f> charuco_corners(to_point_vector(scm_to_pointer(scm_corners), count));
    try {
      cv::aruco::drawDetectedCornersCharuco(img, charuco_corners, charuco_ids, cv::Scalar(255, 0, 0));
    } catch (cv::Exception &e) {
      scm_misc_error("opencv-draw-corners", e.what(), SCM_EOL);
    }
    return SCM_UNDEFINED;
  }

  SCM opencv_draw_detected_markers(SCM scm_shape, SCM scm_type, SCM scm_image, SCM scm_count, SCM scm_ids, SCM scm_markers)
  {
    cv::Mat img(to_cvmat(scm_shape, scm_type, scm_image));
    int count = scm_to_int(scm_count);
    std::vector<int> charuco_ids(to_int_vector(scm_to_pointer(scm_ids), count));
    std::vector<std::vector<cv::Point2f>> marker_corners(to_point_vector_vector(scm_to_pointer(scm_markers), count));
    try {
      cv::aruco::drawDetectedMarkers(img, marker_corners, charuco_ids, cv::Scalar(0, 255, 0));
    } catch (cv::Exception &e) {
      scm_misc_error("opencv-draw-detected-markers", e.what(), SCM_EOL);
    }
    return SCM_UNDEFINED;
  }

  SCM opencv_grid(SCM scm_cols, SCM scm_size, SCM scm_indices, SCM scm_count)
  {
    int cols = scm_to_int(scm_cols);
    float size = scm_to_double(scm_size);
    int count = scm_to_int(scm_count);
    float *result = (float *)scm_gc_malloc_pointerless(count * 3 * sizeof(float), "opencv-grid");
    int *indices = (int *)scm_to_pointer(scm_indices);
    for (int i=0; i<count; i++) {
      result[i * 3    ] = (indices[i] % cols) * size;
      result[i * 3 + 1] = (indices[i] / cols) * size;
      result[i * 3 + 2] = 0.0;
    };
    return scm_from_pointer(result, NULL);
  }

  SCM opencv_camera_calibration(SCM scm_count, SCM scm_sizes, SCM scm_object_points, SCM scm_image_points, SCM scm_image_size)
  {
    float *camera = (float *)scm_gc_malloc_pointerless(3 * 3 * sizeof(double), "camera-matrix");
    float *distortion = (float *)scm_gc_malloc_pointerless(5 * sizeof(double), "distortion");
    int count = scm_to_int(scm_count);
    std::vector<std::vector<cv::Point3f>> object_points(to_point3f_vector_vector(scm_count, scm_sizes, scm_object_points));
    std::vector<std::vector<cv::Point2f>> image_points(to_point2f_vector_vector(scm_count, scm_sizes, scm_image_points));
    cv::Size image_size(scm_to_int(scm_cadr(scm_image_size)), scm_to_int(scm_car(scm_image_size)));
    cv::Mat camera_matrix(3, 3, CV_64FC1, camera);
    cv::Mat dist_coeffs(5, 1, CV_64FC1, distortion);
    std::vector<cv::Mat> rvecs;
    std::vector<cv::Mat> tvecs;
    double error;
    try {
      error = cv::calibrateCamera(object_points, image_points, image_size, camera_matrix, dist_coeffs, rvecs, tvecs);
    } catch (cv::Exception &e) {
      scm_misc_error("opencv-calibrate-camera", e.what(), SCM_EOL);
    }
    return scm_list_3(scm_from_double(error),
                      scm_from_pointer(camera, NULL),
                      scm_from_pointer(distortion, NULL));
  }

  SCM opencv_write_calibration(SCM scm_file_name, SCM scm_intrinsic, SCM scm_distortion)
  {
    cv::FileStorage fs(scm_to_locale_string(scm_file_name), cv::FileStorage::WRITE);
    cv::Mat intrinsic(3, 3, CV_64FC1, scm_to_pointer(scm_intrinsic));
    cv::Mat distortion(5, 1, CV_64FC1, scm_to_pointer(scm_distortion));
    fs << "cameraMatrix" << intrinsic;
    fs << "distCoeffs" << distortion;
    fs.release();
    return SCM_UNDEFINED;
  }

  SCM opencv_read_calibration(SCM scm_file_name)
  {
    cv::FileStorage fs(scm_to_locale_string(scm_file_name), cv::FileStorage::READ);
    float *camera_ptr = (float *)scm_gc_malloc_pointerless(3 * 3 * sizeof(double), "camera-matrix");
    float *distortion_ptr = (float *)scm_gc_malloc_pointerless(5 * sizeof(double), "distortion");
    cv::Mat intrinsic(3, 3, CV_64FC1, camera_ptr);
    cv::Mat distortion(5, 1, CV_64FC1, distortion_ptr);
    fs["cameraMatrix"] >> intrinsic;
    fs["distCoeffs"] >> distortion;
    fs.release();
    return scm_list_2(scm_from_pointer(camera_ptr, NULL),
                      scm_from_pointer(distortion_ptr, NULL));
  }

  void init_opencv(void) {
    scm_c_define("CV_8UC1" , scm_from_int(CV_8UC1 ));
    scm_c_define("CV_8UC3" , scm_from_int(CV_8UC3 ));
    scm_c_define("CV_8SC1" , scm_from_int(CV_8SC1 ));
    scm_c_define("CV_8SC3" , scm_from_int(CV_8SC3 ));
    scm_c_define("CV_16UC1", scm_from_int(CV_16UC1));
    scm_c_define("CV_16UC3", scm_from_int(CV_16UC3));
    scm_c_define("CV_16SC1", scm_from_int(CV_16SC1));
    scm_c_define("CV_16SC3", scm_from_int(CV_16SC3));
    scm_c_define("CV_32SC1", scm_from_int(CV_32SC1));
    scm_c_define("CV_32SC3", scm_from_int(CV_32SC3));
    scm_c_define("CV_32FC1", scm_from_int(CV_32FC1));
    scm_c_define("CV_32FC3", scm_from_int(CV_32FC3));
    scm_c_define("CV_64FC1", scm_from_int(CV_64FC1));
    scm_c_define("CV_64FC3", scm_from_int(CV_64FC3));
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

    scm_c_define_gsubr("opencv-connected-components" ,  6, 0, 0, SCM_FUNC(opencv_connected_components ));
    scm_c_define_gsubr("opencv-charuco-board"        ,  7, 0, 0, SCM_FUNC(opencv_charuco_board        ));
    scm_c_define_gsubr("opencv-draw-marker"          ,  4, 0, 0, SCM_FUNC(opencv_draw_marker          ));
    scm_c_define_gsubr("opencv-detect-markers"       ,  4, 0, 0, SCM_FUNC(opencv_detect_markers       ));
    scm_c_define_gsubr("opencv-interpolate-corners"  , 10, 0, 0, SCM_FUNC(opencv_interpolate_corners  ));
    scm_c_define_gsubr("opencv-draw-corners"         ,  6, 0, 0, SCM_FUNC(opencv_draw_corners         ));
    scm_c_define_gsubr("opencv-draw-detected-markers",  6, 0, 0, SCM_FUNC(opencv_draw_detected_markers));
    scm_c_define_gsubr("opencv-grid"                 ,  4, 0, 0, SCM_FUNC(opencv_grid                 ));
    scm_c_define_gsubr("opencv-camera-calibration"   ,  5, 0, 0, SCM_FUNC(opencv_camera_calibration   ));
    scm_c_define_gsubr("opencv-write-calibration"    ,  3, 0, 0, SCM_FUNC(opencv_write_calibration    ));
    scm_c_define_gsubr("opencv-read-calibration"     ,  1, 0, 0, SCM_FUNC(opencv_read_calibration     ));
  };
}
