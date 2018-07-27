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
(define-module (aiscm xorg)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 optargs)
  #:use-module (aiscm util)
  #:use-module (aiscm core)
  #:use-module (aiscm image)
  #:export (<xdisplay> <meta<xdisplay>>
            <xwindow> <meta<xwindow>>
            process-events event-loop quit? quit= show show-fullscreen hide title= move resize move-resize window-size
            fullscreen-flag xorg-io-type IO-XIMAGE IO-OPENGL IO-XVIDEO write-image)
  #:re-export (destroy))


(load-extension "libguile-aiscm-xorg" "init_xorg")
(define-class* <xdisplay> <object> <meta<xdisplay>> <class>
  (display #:init-keyword #:display #:getter get-display))
(define-method (initialize (self <xdisplay>) initargs)
  (let-keywords initargs #f (name)
    (let [(name (or name ":0.0"))]
      (next-method self (list #:display (make-display name))))))
(define-method (shape (self <xdisplay>)) (display-shape (get-display self)))
(define-method (process-events (self <xdisplay>)) (display-process-events (get-display self)))
(define-method (event-loop (self <xdisplay>) timeout) (display-event-loop (get-display self) timeout))
(define-method (event-loop (self <xdisplay>)) (cut event-loop self <>))
(define-method (destroy (self <xdisplay>)) (display-destroy (get-display self)))
(define-method (quit? (self <xdisplay>)) (display-quit? (get-display self)))
(define-method (quit= (self <xdisplay>) (value <boolean>)) (display-quit= (get-display self) value))
(define-class* <xwindow> <object> <meta<xwindow>> <class>
              (window #:init-keyword #:window #:getter get-window))
(define-method (initialize (self <xwindow>) initargs)
  (let-keywords initargs #f (display shape io)
    (let [(io     (or io IO-XIMAGE))]
      (next-method self (list #:window (make-window (get-display display) (car shape) (cadr shape) io))))))

(define (window-size img . args)
  "Determine window size for an image and some optional keyword arguments"
  (let* [(shp (shape img))
         (w   (car shp))
         (h   (cadr shp))]
    (or (let-keywords args #t (shape width height)
      (or shape
          (and width  (list width (round (* (/ width w) h))))
          (and height (list (round (* (/ height h) w)) height))))
    shp)))

(define-syntax-rule (flag? name args)
  (let-keywords args #t (name) name))

(define (fullscreen-flag . args)
  "Check whether fullscreen keyword is set to true"
  (flag? fullscreen args))

(define (xorg-io-type is-video images . args)
  "Select X.Org IO type"
  (let-keywords args #t (io)
    (or io (if is-video (if (null? (cdr images)) IO-XVIDEO IO-OPENGL) IO-XIMAGE))))

(define-method (show (self <xwindow>))
  (window-show (get-window self)))
(define-method (show (self <image>) . args) (apply show (list self) args) self)
(define-method (show (self <multiarray<>>) . args) (apply show (list self) args) self)
(define-method (show (self <list>) . args)
  (let* [(dsp         (make <xdisplay>))
         (io          (apply xorg-io-type #f self args))
         (images      (map to-image self))
         (shapes      (map (cut apply window-size <> args) images))
         (fullscreen  (apply fullscreen-flag args))
         (show-method(if fullscreen show-fullscreen show))
         (window      (cut make <xwindow> #:display dsp #:shape <> #:io io))
         (windows     (map window shapes))]
    (for-each (cut title= <> "AIscm") windows)
    (for-each write-image images windows)
    (for-each show-method windows)
    (event-loop dsp #f)
    (for-each hide windows)
    (destroy dsp)
    self))
(define-method (show (self <procedure>) . args)
  (let* [(dsp         (make <xdisplay>))
         (result      (self dsp))
         (results     (if (list? result) result (list result)))
         (io          (apply xorg-io-type #t results args))
         (images      (map to-image results))
         (shapes      (map (cut apply window-size <> args) images))
         (fullscreen  (apply fullscreen-flag args))
         (show-method (if fullscreen show-fullscreen show))
         (window      (cut make <xwindow> #:display dsp #:shape <> #:io io))
         (windows     (map window shapes))]
    (for-each (cut title= <> "AIscm") windows)
    (for-each write-image images windows)
    (for-each show-method windows)
    (while (not (quit? dsp))
      (set! result (self dsp))
      (if result
        (begin
          (set! results (if (list? result) result (list result)))
          (set! images (map to-image results))
          (for-each write-image images windows)
          (process-events dsp))
        (quit= dsp #t)))
    (for-each hide windows)
    (destroy dsp)
    result))

(define-method (show-fullscreen (self <xwindow>)) (window-show-fullscreen (get-window self)))

(define-method (hide (self <xwindow>)) (window-hide (get-window self)))
(define-method (destroy (self <xwindow>)) (window-destroy (get-window self)))
(define-method (title= (self <xwindow>) (title <string>)) (window-title= (get-window self) title))

(define-method (move (self <xwindow>) (x <integer>) (y <integer>))
  (window-move (get-window self) x y))
(define-method (resize (self <xwindow>) (w <integer>) (h <integer>))
  (window-resize (get-window self) w h))
(define-method (move-resize (self <xwindow>) (x <integer>) (y <integer>) (w <integer>) (h <integer>))
  (window-move-resize (get-window self) x y w h))

(define-method (write-image (image <image>) (self <xwindow>)) (window-write (get-window self) image))
(define-method (write-image (arr <multiarray<>>) (self <xwindow>)) (show self (to-image arr)) arr)
