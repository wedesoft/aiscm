(define-module (aiscm ffmpeg)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (aiscm mem)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm sequence)
  #:use-module (aiscm float)
  #:use-module (aiscm mem)
  #:use-module (aiscm image)
  #:use-module (aiscm util)
  #:export (<ffmpeg> open-input-video open-input-audio
            read-video read-audio frame-rate video-pts audio-pts))

(load-extension "libguile-aiscm-ffmpeg" "init_ffmpeg")

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
  (let [(picture (format-context-read-video (slot-ref self 'format-context)))
        (memory  (lambda (data size) (make <mem> #:base data #:size size)))]
    (and picture
         (apply (lambda (tag format shape offsets pitches data size)
                  (make <image>
                        #:format  (format->symbol format)
                        #:shape   shape
                        #:offsets offsets
                        #:pitches pitches
                        #:mem     (memory data size)))
                picture))))

(define (audio-pts self) (format-context-audio-pts (slot-ref self 'format-context)))
(define (read-audio self)
  (let [(samples    (format-context-read-audio (slot-ref self 'format-context)))
        (memory     (lambda (data size) (make <mem> #:base data #:size size)))
        (array-type (lambda (type) (multiarray (audio-format->type type) 2)))
        (array      (lambda (array-type shape memory) (make array-type #:shape shape #:value memory)))]
    (and samples
         (apply (lambda (tag type shape data size)
                  (array (array-type type) shape (memory data size)))
                samples))))

(define-method (channels (self <ffmpeg>)) (format-context-channels (slot-ref self 'format-context)))
(define-method (rate (self <ffmpeg>)) (format-context-rate (slot-ref self 'format-context)))
(define-method (typecode (self <ffmpeg>))
  (audio-format->type (format-context-typecode (slot-ref self 'format-context))))
