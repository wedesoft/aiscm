(define-module (aiscm ffmpeg)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm util)
  #:export (<ffmpeg> open-input-video read-video))

(load-extension "libguile-ffmpeg" "init_ffmpeg")

(define-class* <ffmpeg> <object> <meta<ffmpeg>> <class>
               (format-context #:init-keyword #:format-context))

(define (open-input-video file-name)
  (make <ffmpeg> #:format-context (open-format-context file-name (equal? "YES" (getenv "DEBUG")))))

(define-method (shape (self <ffmpeg>)) (format-context-shape (slot-ref self 'format-context)))
(define (read-video self) (format-context-read-video (slot-ref self 'format-context)))
