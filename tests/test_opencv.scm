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
(define color-img (to-array (convert-image (to-image img) 'RGB)))
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
    'misc-error (draw-corners (make (multiarray <ubyte> 2) #:shape '(0 0)) corners)))

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
    '(5) (shape (caddr (camera-calibration (list object-points) (list image-points) '(320 240))))))

(test-end "aiscm opencv")