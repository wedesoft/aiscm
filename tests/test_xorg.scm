;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 Jan Wedekind <jan@wedesoft.de>
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

(define img (make <image> #:format 'GRAY #:shape '(240 320)))
(test-begin "window-size")
  (test-equal "Use shape of image"
    '(240 320) (window-size img))
  (test-equal "Override window size"
    '(480 640) (window-size img #:shape '(480 640)))
  (test-equal "Infer height of scaled display"
    '(480 640) (window-size img #:width 640))
  (test-equal "Infer height of non-scaled display"
    '(240 320) (window-size img #:width 320))
  (test-equal "Round height to nearest integer"
    '(22 30) (window-size img #:width 30))
  (test-equal "Infer height of scaled display"
    '(480 640) (window-size img #:width 640))
  (test-equal "Infer width of scaled display"
    '(480 640) (window-size img #:height 480))
  (test-equal "Infer width of non-scaled display"
    '(240 320) (window-size img #:height 240))
  (test-equal "Round height to nearest integer"
    '(22 29) (window-size img #:height 22))
  (test-equal "Ignore other keyword arguments"
    '(240 320) (window-size img #:fullscreen #t))
(test-end "window-size")

(test-begin "fullscreen-flag")
  (test-assert "Fullscreen is disabled by default"
    (not (fullscreen-flag)))
  (test-assert "Enable fullscreen"
    (fullscreen-flag #:fullscreen #t))
  (test-assert "Explicitely disable fullscreen"
    (not (fullscreen-flag #:fullscreen #f)))
  (test-assert "Ignore other keyword arguments"
    (not (fullscreen-flag #:width 640)))
(test-end "fullscreen-flag")

(test-begin "xorg-io-type")
  (test-eq "default for static image is XImage"
    IO-XIMAGE (xorg-io-type #f '(img)))
  (test-eq "default for video is XVideo"
    IO-XVIDEO (xorg-io-type #t '(img)))
  (test-eq "default for multiple videos is OpenGL"
    IO-OPENGL (xorg-io-type #t '(img1 img2)))
  (test-eq "override the default using a keyword argument"
    IO-XVIDEO (xorg-io-type #f '(img) #:io IO-XVIDEO))
  (test-eq "ignore other keyword arguments"
    IO-XIMAGE (xorg-io-type #f '(img) #:fullscreen #t))
(test-end "xorg-io-type")
(test-end "aiscm xorg")
