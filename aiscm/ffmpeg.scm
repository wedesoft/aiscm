(define-module (aiscm ffmpeg)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm mem)
  #:use-module (aiscm image)
  #:use-module (aiscm util)
  #:export (<ffmpeg> open-input-video read-video frame-rate))

(load-extension "libguile-ffmpeg" "init_ffmpeg")

(define-class* <ffmpeg> <object> <meta<ffmpeg>> <class>
               (format-context #:init-keyword #:format-context))

(define (open-input-video file-name)
  (make <ffmpeg> #:format-context (open-format-context file-name (equal? "YES" (getenv "DEBUG")))))

(define-method (shape (self <ffmpeg>)) (format-context-shape (slot-ref self 'format-context)))
(define (frame-rate self) (format-context-frame-rate (slot-ref self 'format-context)))
(define (read-video self)
  (let [(picture (format-context-read-video (slot-ref self 'format-context)))]
    (and picture
         (make <image>
               #:format  (format->symbol (car picture))
               #:shape   (cadr picture)
               #:offsets (caddr picture)
               #:pitches (cadddr picture)
               #:mem     (make <mem> #:base (last picture) #:size 1000000)))))
