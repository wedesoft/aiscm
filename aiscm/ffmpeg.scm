(define-module (aiscm ffmpeg)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm sequence)
  #:use-module (aiscm float)
  #:use-module (aiscm mem)
  #:use-module (aiscm image)
  #:use-module (aiscm util)
  #:export (<ffmpeg> open-input-video open-input-audio
            read-video read-audio frame-rate video-pts channels rate))

(load-extension "libguile-ffmpeg" "init_ffmpeg")

(define-class* <ffmpeg> <object> <meta<ffmpeg>> <class>
               (format-context #:init-keyword #:format-context))
(define audio-formats
  (list (cons <ubyte>  AV_SAMPLE_FMT_U8P )
        (cons <sint>   AV_SAMPLE_FMT_S16P)
        (cons <int>    AV_SAMPLE_FMT_S32P)
        (cons <float>  AV_SAMPLE_FMT_FLTP)
        (cons <double> AV_SAMPLE_FMT_DBLP)))
(define audio-types (alist-invert audio-formats))
(define (audio-format->type fmt) (assq-ref audio-types fmt))

(define (open-input file-name)
  (make <ffmpeg> #:format-context (open-format-context file-name (equal? "YES" (getenv "DEBUG")))))
(define (open-input-video file-name) (open-input file-name))
(define (open-input-audio file-name) (open-input file-name))

(define-method (shape (self <ffmpeg>)) (format-context-shape (slot-ref self 'format-context)))
(define (frame-rate self) (format-context-frame-rate (slot-ref self 'format-context)))

(define (video-pts self) (format-context-video-pts (slot-ref self 'format-context)))
(define (read-video self)
  (let [(picture (format-context-read-video (slot-ref self 'format-context)))]
    (and picture
         (make <image>
               #:format  (format->symbol (car picture))
               #:shape   (cadr picture)
               #:offsets (caddr picture)
               #:pitches (cadddr picture)
               #:mem     (make <mem> #:base (last picture) #:size (list-ref picture 4))))))

(define (read-audio self)
  (make (multiarray (typecode self) 2) #:shape (list (channels self) 100)))

(define (channels self) (format-context-channels (slot-ref self 'format-context)))
(define (rate self) (format-context-rate (slot-ref self 'format-context)))
(define-method (typecode (self <ffmpeg>))
  (audio-format->type (format-context-typecode (slot-ref self 'format-context))))
