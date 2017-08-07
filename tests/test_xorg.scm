;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Wedekind <jan@wedesoft.de>
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
             (aiscm xorg)
             (aiscm image))

(test-begin "aiscm xorg")

(define img (make <image> #:format 'GRAY #:shape '(320 240)))
(test-begin "window-size")
  (test-equal "Use shape of image"
    '(320 240) (window-size img))
  (test-equal "Override window size"
    '(640 480) (window-size img #:shape '(640 480)))
  (test-equal "Infer height of scaled display"
    '(640 480) (window-size img #:width 640))
  (test-equal "Infer height of non-scaled display"
    '(320 240) (window-size img #:width 320))
  (test-equal "Round height to nearest integer"
    '(30 22) (window-size img #:width 30))
  (test-equal "Infer height of scaled display"
    '(640 480) (window-size img #:width 640))
  (test-equal "Infer width of scaled display"
    '(640 480) (window-size img #:height 480))
  (test-equal "Infer width of non-scaled display"
    '(320 240) (window-size img #:height 240))
  (test-equal "Round height to nearest integer"
    '(29 22) (window-size img #:height 22))
  (test-equal "Ignore other arguments"
    '(320 240) (window-size img #:borderless #t))
(test-end "window-size")

(test-begin "borderless-flag")
  (test-assert "Border is enabled by default"
    (not (borderless-flag)))
  (test-assert "Disable border"
    (borderless-flag #:borderless #t))
  (test-assert "Explicitely enable border"
    (not (borderless-flag #:borderless #f)))
  (test-assert "Ignore other arguments"
    (borderless-flag #:borderless #t #:width 320))
(test-end "borderless-flag")
(test-end "aiscm xorg")
