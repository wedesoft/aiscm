;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016 Jan Wedekind <jan@wedesoft.de>
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
(define-module (aiscm v4l2)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (aiscm util)
  #:use-module (aiscm element)
  #:use-module (aiscm mem)
  #:use-module (aiscm int)
  #:use-module (aiscm image)
  #:use-module (aiscm sequence)
  #:use-module (system foreign)
  #:export (<v4l2> <meta<v4l2>>)
  #:re-export (destroy read-image))

(load-extension "libguile-aiscm-v4l2" "init_v4l2")

(define-class* <v4l2> <object> <meta<v4l2>> <class>
               (videodev2 #:init-keyword #:videodev2))

(define formats
  (list (cons 'RGB  V4L2_PIX_FMT_RGB24 )
        (cons 'BGR  V4L2_PIX_FMT_BGR24 )
        (cons 'I420 V4L2_PIX_FMT_YUV420)
        (cons 'UYVY V4L2_PIX_FMT_UYVY  )
        (cons 'YUY2 V4L2_PIX_FMT_YUYV  )
        (cons 'GRAY V4L2_PIX_FMT_GREY  )
        (cons 'MJPG V4L2_PIX_FMT_MJPEG )))
(define symbols (alist-invert formats))
(define (symbol->v4l2-format sym) (assq-ref formats sym))
(define (v4l2-format->symbol fmt) (assq-ref symbols fmt))
(define (supported? fmt)
  (or (v4l2-format->symbol (car fmt))
      (begin (warn (format #f "Unsupported V4L2 format 0x~x" (car fmt))) #f)))
(define format-order (map car formats))
(define (format< x y)
  (let [(ord-x (index-of (car x) format-order))
        (ord-y (index-of (car y) format-order))
        (size-x (apply * (cdr x)))
        (size-y (apply * (cdr y)))]
    (or (< ord-x ord-y) (and (= ord-x ord-y) (< size-x size-y)))))

(define-method (initialize (self <v4l2>) initargs)
  (let-keywords initargs #f (device channel select)
    (let* [(device    (or device "/dev/video0"))
           (channel   (or channel 0))
           (select    (or select last))
           (decode    (lambda (f) (cons (v4l2-format->symbol (car f)) (cdr f))))
           (encode    (lambda (f) (cons (symbol->v4l2-format (car f)) (cdr f))))
           (selection (lambda (formats)
                        (encode (select (sort (map decode (filter supported? formats)) format<)))))]
      (next-method self (list #:videodev2 (make-videodev2 device channel selection))))))

(define-method (destroy (self <v4l2>)) (videodev2-destroy (slot-ref self 'videodev2)))

(define-method (shape (self <v4l2>)) (videodev2-shape (slot-ref self 'videodev2)))

(define-method (read-image (self <v4l2>))
  (let [(picture (videodev2-read-image (slot-ref self 'videodev2)))
        (memory  (lambda (base size) (make <mem> #:base base #:size size)))]
    (apply (lambda (format shape base size)
             (make <image>
                   #:format (v4l2-format->symbol format)
                   #:shape shape
                   #:mem (memory base size)))
           picture)))
