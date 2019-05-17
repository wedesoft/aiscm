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
(define-module (aiscm image)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 optargs)
  #:use-module (system foreign)
  #:use-module (aiscm util)
  #:use-module (aiscm core)
  #:export (<image> <meta<image>>
            get-format convert-image to-image symbol->format format->symbol convert-image-from! from-image))


(load-extension "libguile-aiscm-image" "init_image")

(define-class* <image> <object> <meta<image>> <class>
              (format       #:init-keyword #:format       #:getter get-format )
              (shape        #:init-keyword #:shape        #:getter shape      )
              (offsets      #:init-keyword #:offsets      #:getter offsets    )
              (pitches      #:init-keyword #:pitches      #:getter pitches    )
              (memory       #:init-keyword #:memory       #:getter memory     )
              (memory-base  #:init-keyword #:memory-base  #:getter memory-base))

(define-method (initialize (self <image>) initargs)
  "Constructor for images"
  (let-keywords initargs #f (format shape offsets pitches memory memory-base)
    (let* [(pitches (or pitches (default-pitches format (cadr shape))))
           (offsets (or offsets (default-offsets format pitches (car shape))))]
      (next-method self (list #:format format
                              #:shape shape
                              #:offsets offsets
                              #:pitches pitches
                              #:memory memory
                              #:memory-base (or memory-base memory))))))

(define formats
  (list (cons 'RGB  AV_PIX_FMT_RGB24)
        (cons 'BGR  AV_PIX_FMT_BGR24)
        (cons 'BGRA AV_PIX_FMT_BGRA)
        (cons 'GRAY AV_PIX_FMT_GRAY8)
        (cons 'I420 AV_PIX_FMT_YUV420P)
        (cons 'YV12 AV_PIX_FMT_YUV420P)
        (cons 'UYVY AV_PIX_FMT_UYVY422)
        (cons 'YUY2 AV_PIX_FMT_YUYV422)
        (cons 'MJPG 0)))

(define symbols (alist-invert formats))

(define (symbol->format sym) (assq-ref formats sym))

(define (format->symbol fmt) (assq-ref symbols fmt))

(define (image-size format pitches height)
  (case format
    ((RGB)  (* (car pitches) height))
    ((BGR)  (* (car pitches) height))
    ((BGRA) (* (car pitches) height))
    ((GRAY) (* (car pitches) height))
    ((I420) (+ (* (car pitches) height) (* 2 (cadr pitches) (ash (+ height 1) -1))))
    ((YV12) (+ (* (car pitches) height) (* 2 (cadr pitches) (ash (+ height 1) -1))))
    ((UYVY) (* (car pitches) height 2))
    ((YUY2) (* (car pitches) height 2))
    ((MJPG) (* (car pitches) height 2))))

(define (default-offsets format pitches height)
  (case format
    ((RGB)  (list 0))
    ((BGR)  (list 0))
    ((BGRA) (list 0))
    ((GRAY) (list 0))
    ((I420) (list 0
                  (* (car pitches) height)
                  (+ (* (car pitches) height) (* (cadr pitches) (ash (+ height 1) -1)))))
    ((YV12) (list 0
                  (* (car pitches) height)
                  (+ (* (car pitches) height) (* (cadr pitches) (ash (+ height 1) -1)))))
    ((UYVY) (list 0))
    ((YUY2) (list 0))
    ((MJPG) (list 0))))

(define (default-pitches format width)
  (case format
    ((RGB)  (list (* width 3)))
    ((BGR)  (list (* width 3)))
    ((BGRA) (list (* width 4)))
    ((GRAY) (list width))
    ((I420) (list width (ash (+ width 1) -1) (ash (+ width 1) -1)))
    ((YV12) (list width (ash (+ width 1) -1) (ash (+ width 1) -1)))
    ((UYVY) (list (* 2 (logand (+ width 3) (lognot #x3)))))
    ((YUY2) (list (* 2 (logand (+ width 3) (lognot #x3)))))
    ((MJPG) (list))))

(define (warp lst indices) (map (cut list-ref lst <>) indices))

(define-method (descriptor (format <symbol>) (shape <list>) (offsets <list>) (pitches <list>))
  (list (symbol->format format)
        shape
        (if (eqv? format 'YV12) (warp offsets '(0 2 1)) offsets)
        (if (eqv? format 'YV12) (warp pitches '(0 2 1)) pitches)))

(define-method (descriptor (self <image>))
  (descriptor (get-format self)
              (shape self)
              (offsets self)
              (pitches self)))

(define-method (convert-image-from! (self <image>) (source <image>))
  "Convert image and write it to the specified target location"
  (let [(dest-type   (descriptor self))
        (source-type (descriptor source))]
    (if (eq? (get-format source) 'MJPG)
      (if (and (eq? (get-format self) 'YV12)
               (equal? (shape self) (shape source))
               (equal? (pitches self) (default-pitches 'YV12 (cadr (shape self)))))
        (mjpeg-to-yuv420p (memory source) (shape self) (memory self) (offsets self))
        (convert-image-from! self (convert-image source 'YV12)))
      (image-convert (memory source) source-type (memory self) dest-type))
    self))

(define-method (convert-image (self <image>) (fmt <symbol>) (shape <list>) (offsets <list>) (pitches <list>))
  "Convert image using the specified attributes"
  (let* [(dest-size   (image-size fmt pitches (car shape)))
         (mem         (gc-malloc-pointerless dest-size))
         (destination (make <image> #:format fmt
                                    #:shape shape
                                    #:memory mem
                                    #:memory-base mem
                                    #:offsets offsets
                                    #:pitches pitches))]
        (convert-image-from! destination self)))

(define-method (convert-image (self <image>) (format <symbol>) (shape <list>))
  (let* [(pitches (default-pitches format (cadr shape)))
         (offsets (default-offsets format pitches (car shape)))]
    (convert-image self format shape offsets pitches)))

(define-method (convert-image (self <image>) (format <symbol>))
  (convert-image self format (shape self)))

(define-method (duplicate (self <image>)) (convert-image self (get-format self)))

(define-method (write (self <image>) port)
  (format port "#<<image> ~a ~a>" (get-format self) (shape self)))

(define (from-image self)
  (case (get-format self)
    ((GRAY) (let* [(shape   (shape self))
                   (pitches (pitches self))
                   (size    (image-size 'GRAY pitches (car shape)))
                   (mem     (memory self))
                   (base    (memory-base self))]
              (make (multiarray <ubyte> 2) #:memory mem #:memory-base base #:shape shape #:strides (list (car pitches) 1))))
    ((RGB)  (let* [(shape   (shape self))
                   (pitches (pitches self))
                   (size    (image-size 'BGR pitches (car shape)))
                   (mem     (memory self))
                   (base    (memory-base self))]
              (make (multiarray <rgb<ubyte>> 2) #:memory mem #:memory-base base #:shape shape #:strides (list (car pitches) 3))))
    (else   (from-image (convert-image self 'RGB)))))

(define-method (to-image (self <image>)) self)

(define-method (to-image (self <multiarray<>>))
  (cond ((equal? <ubyte> (typecode self))
         (if (= (cadr (strides self)) 1)
           (make <image> #:format       'GRAY
                         #:shape        (shape self)
                         #:memory       (memory self)
                         #:memory-base  (memory-base self)
                         #:offsets      '(0)
                         #:pitches      (list (car (strides self))))
           (to-image (duplicate self))))
        ((or (is-a? (typecode self) <meta<int<>>>) (is-a? (typecode self) <meta<float<>>>))
         (to-image (to-type <ubyte> self)))
        ((equal? <rgb<ubyte>> (typecode self))
         (if (= (cadr (strides self)) 3)
           (make <image> #:format      'RGB
                         #:shape       (shape self)
                         #:memory      (memory self)
                         #:memory-base (memory-base self)
                         #:offsets     '(0)
                         #:pitches     (list (car (strides self))))
           (to-image (duplicate self))))
        (else (to-image (to-type <rgb<ubyte>> self)))))
