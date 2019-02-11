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
(define-module (aiscm opencv)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (aiscm core)
  #:export (connected-components charuco-board
            DICT_4X4_50 DICT_4X4_50 DICT_4X4_100 DICT_4X4_250 DICT_4X4_1000 DICT_5X5_50 DICT_5X5_100 DICT_5X5_250 DICT_5X5_1000
            DICT_6X6_50 DICT_6X6_100 DICT_6X6_250 DICT_6X6_1000 DICT_7X7_50 DICT_7X7_100 DICT_7X7_250 DICT_7X7_1000
            DICT_ARUCO_ORIGINAL DICT_APRILTAG_16h5 DICT_APRILTAG_25h9 DICT_APRILTAG_36h10 DICT_APRILTAG_36h11))

(load-extension "libguile-aiscm-opencv" "init_opencv")

(define typemap
  (list (cons <ubyte> CV_8UC1 )
        (cons <byte>  CV_8SC1 )
        (cons <usint> CV_16UC1)
        (cons <sint>  CV_16SC1)
        (cons <int>   CV_32SC1)))

(define* (connected-components img connectivity #:key (label-type <int>))
  (let* [(result (make (multiarray label-type 2) #:shape (shape img)))
         (count  (opencv-connected-components (memory img) (memory result) (shape img) connectivity (assq-ref typemap label-type)))]
    (cons result count)))

(define (charuco-board rows cols size marker-size dict)
  (let [(result (make (multiarray <ubyte> 2) #:shape (list (* rows size) (* cols size))))]
    (opencv-charuco-board (memory result) rows cols size marker-size dict)
    result))
