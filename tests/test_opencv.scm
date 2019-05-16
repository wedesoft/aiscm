;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018, 2019 Jan Wedekind <jan@wedesoft.de>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
(use-modules (srfi srfi-64)
             (oop goops)
             (aiscm core)
             (aiscm image)
             (aiscm opencv))

(test-begin "aiscm opencv")

(test-group "connected components"
  (test-equal "connected components of unsigned byte array"
    '((1 0 0) (0 0 2)) (to-list (car (connected-components (arr (1 0 0) (0 0 1)) 8))))
  (test-eqv "count number of connected components"
    3 (cdr (connected-components (arr (1 0 0) (0 0 1)) 8)))
  (test-eq "return integer array by default"
    <int> (typecode (car (connected-components (arr (1 0 0) (0 0 1)) 8))))
  (test-eq "return unsigned short int if requested"
    <usint> (typecode (car (connected-components (arr (1 0 0) (0 0 1)) 8 #:label-type <usint>))))
  (test-equal "connected components with unsigned short int result"
    '((1 0 0) (0 0 2)) (to-list (car (connected-components (arr (1 0 0) (0 0 1)) 8 #:label-type <usint>))))
  (test-error "throw error if label type is unsupported"
    'misc-error (connected-components (arr (1 0 0) (0 0 1)) 8 #:label-type <byte>)))

(test-group "Generate Charuco board"
  (test-equal "shape of Charuco board"
    '(500 700) (shape (charuco-board 5 7 100 50 DICT_4X4_50)))
  (test-error "throw error if board has wrong parameters"
    'misc-error (charuco-board 5 7 100 150 DICT_4X4_50))
  (test-equal "draw single Aruco marker"
    '(128 128) (shape (draw-marker DICT_4X4_50 0 128)))
  (test-error "throw exception if Aruco marker is too small"
    'misc-error (draw-marker DICT_4X4_50 0 0)))

(define img (charuco-board 5 7 100 50 DICT_4X4_50))
(define color-img (from-image (convert-image (to-image img) 'RGB)))
(define aruco (detect-markers img DICT_4X4_50))
(define color-aruco (detect-markers color-img DICT_4X4_50))
(define corners (interpolate-corners aruco img 5 7 100 50))
(test-group "Detect Aruco markers"
  (test-equal "shape of marker identity array"
    '(17) (shape (car aruco)))
  (test-equal "shape of corner array"
    '(17 4 2) (shape (cdr aruco)))
  (test-equal "shape of charuco corner identity array"
    '(24) (shape (car corners)))
  (test-equal "shape of charuco corner array"
    '(24 2) (shape (cdr corners)))
  (test-equal "detect markers in color image"
    '(17) (shape (car color-aruco)))
  (test-error "throw exception if detection image is of wrong type"
    'misc-error (detect-markers (make (multiarray <int> 2) #:shape '(320 240)) DICT_4X4_50))
  (test-equal "interpolate Charuco corners in color image"
    '(24) (shape (car (interpolate-corners color-aruco color-img 5 7 100 50))))
  (test-error "throw error if no markers defined"
    'misc-error (interpolate-corners (cons (make (multiarray <int> 1) #:shape '(0))
                                           (make (multiarray <float> 3) #:shape '(0 4 2)))
                                     img 5 7 100 50))
  (test-equal "shape of Charuco corner image"
    '(500 700) (shape (draw-corners img corners)))
  (test-error "throw exception when drawing Charuco corners does not work"
    'misc-error (draw-corners (make (multiarray <ubyte> 2) #:shape '(0 0)) corners))
  (test-error "check type of marker ids"
    'misc-error (draw-corners img (cons (to-type <sint> (car corners)) (cdr corners))))
  (test-error "check type of marker corners"
    'misc-error (draw-corners img (cons (car corners) (to-type <double> (cdr corners)))))
  (test-equal "draw detected markers"
    '(500 700) (shape (draw-detected-markers img aruco)))
  (test-error "error handling when drawing detected markers"
    'misc-error (draw-detected-markers (make (multiarray (rgb <ubyte>) 2) #:shape '(0 0)) aruco))
  (test-error "check marker id type"
    'misc-error (draw-detected-markers img (cons (to-type <sint> (car aruco)) (cdr aruco))))
  (test-error "check marker corner type"
    'misc-error (draw-detected-markers img (cons (car aruco) (to-type <double> (cdr aruco)))))
  (test-error "check type of marker ids"
    'misc-error (interpolate-corners (cons (to-type <sint> (car aruco)) (cdr aruco)) img 5 7 100 50))
  (test-error "check type of marker coordinates"
    'misc-error (interpolate-corners (cons (car aruco) (to-type <double> (cdr aruco))) img 5 7 100 50)))

(define object-points (arr <float> (0 0 0) (0 1 0) (1 0 0) (1 1 0)))
(define image-points (arr <float> (0 0) (0 1) (1 0) (1 1)))
(test-group "camera calibration"
  (test-equal "grid with only one corner"
    '((0.0 0.0 0.0)) (to-list (grid 7 0.25 (arr 0))))
  (test-equal "grid with second corner"
    '((0.25 0.0 0.0)) (to-list (grid 7 0.25 (arr 1))))
  (test-equal "second row of grid"
    '((0.0 0.25 0.0)) (to-list (grid 7 0.25 (arr 7))))
  (test-error "error if no object points specified"
    'misc-error (camera-calibration '() '() '(320 240)))
  (test-equal "shape of camera matrix"
    '(3 3) (shape (cadr (camera-calibration (list object-points) (list image-points) '(320 240)))))
  (test-equal "shape of distortion coefficients"
    '(5) (shape (caddr (camera-calibration (list object-points) (list image-points) '(320 240)))))
  (test-error "check type of object points"
    'misc-error (camera-calibration (list (to-type <double> object-points)) (list image-points) '(320 240)))
  (test-error "check type of image points"
    'misc-error (camera-calibration (list object-points) (list (to-type <double> image-points)) '(320 240))))

(define cal-file-name (string-append (tmpnam) ".yml"))
(define intrinsic (arr <double> (680.0 0.0 320.0) (0.0 680.0 240.0) (0.0 0.0 1.0)))
(define distortion (arr <double> 0.25 0.5 0.75 1.0 1.25))
(write-camera-calibration cal-file-name intrinsic distortion)
(test-group "save/load camera calibration"
  (test-equal "read camera intrinsic matrix"
    (to-list intrinsic) (to-list (car (read-camera-calibration cal-file-name))))
  (test-equal "read distortion coefficients"
    (to-list distortion) (to-list (cdr (read-camera-calibration cal-file-name))))
  (test-error "throw exception when failing to write camera calibration"
    'misc-error (write-camera-calibration "nosuchdir/test.yml" intrinsic distortion))
  (test-error "throw exception when failing to read a camera calibration file"
    'misc-error (read-camera-calibration "nosuchfile.yml"))
  (test-error "check type of camera matrix"
    'misc-error (write-camera-calibration cal-file-name (to-type <float> intrinsic) distortion))
  (test-error "check type of distortion coefficients"
    'misc-error (write-camera-calibration cal-file-name intrinsic (to-type <float> distortion))))

(define corners (arr <float> ((0 0) (0 1) (1 0) (1 1))))
(define img (make (multiarray (rgb <ubyte>) 2) #:shape '(240 320)))
(test-group "estimate pose of markers"
  (test-equal "return array of rotation vectors"
    '(1 3) (shape (car (estimate-pose-single-markers corners 1.0 intrinsic distortion))))
  (test-equal "return array of rotation vectors"
    '(1 3) (shape (cdr (estimate-pose-single-markers corners 1.0 intrinsic distortion))))
  (test-error "check type of corners array"
    'misc-error (estimate-pose-single-markers (to-type <double> corners) 1.0 intrinsic distortion))
  (test-error "check type of camera matrix"
    'misc-error (estimate-pose-single-markers corners 1.0 (to-type <float> intrinsic) distortion))
  (test-error "check type of distortion coefficients"
    'misc-error (estimate-pose-single-markers corners 1.0 intrinsic (to-type <float> distortion)))
  (test-equal "draw axes"
    '(240 320) (shape (draw-axis img intrinsic distortion (arr <double> 0 0 0) (arr <double> 0 0 0) 0.25)))
  (test-error "check type of camera matrix"
    'misc-error (draw-axis img (to-type <float> intrinsic) distortion (arr <double> 0 0 0) (arr <double> 0 0 0) 0.25))
  (test-error "check type of distortion coefficients"
    'misc-error (draw-axis img intrinsic (to-type <float> distortion) (arr <double> 0 0 0) (arr <double> 0 0 0) 0.25))
  (test-error "check type of rotation vectors"
    'misc-error (draw-axis img intrinsic distortion (arr <float> 0 0 0) (arr <double> 0 0 0) 0.25))
  (test-error "check type of translation vectors"
    'misc-error (draw-axis img intrinsic distortion (arr <double> 0 0 0) (arr <float> 0 0 0) 0.25)))

(test-end "aiscm opencv")
