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
               (ffmpeg #:init-keyword #:ffmpeg))
(define audio-formats
  (list (cons <ubyte>  AV_SAMPLE_FMT_U8P )
        (cons <sint>   AV_SAMPLE_FMT_S16P)
        (cons <int>    AV_SAMPLE_FMT_S32P)
        (cons <float>  AV_SAMPLE_FMT_FLTP)
        (cons <double> AV_SAMPLE_FMT_DBLP)))
(define audio-types (alist-invert audio-formats))
(define (audio-format->type fmt) (assq-ref audio-types fmt))

(define (open-input file-name)
  (make <ffmpeg> #:ffmpeg (open-ffmpeg file-name (equal? "YES" (getenv "DEBUG")))))
(define (open-input-video file-name) (open-input file-name))
(define (open-input-audio file-name) (open-input file-name))

(define-method (shape (self <ffmpeg>)) (ffmpeg-shape (slot-ref self 'ffmpeg)))
(define (frame-rate self) (ffmpeg-frame-rate (slot-ref self 'ffmpeg)))

(define (import-audio-frame lst)
  (let [(memory     (lambda (data size) (make <mem> #:base data #:size size)))
        (array-type (lambda (type) (multiarray (audio-format->type type) 2)))
        (array      (lambda (array-type shape memory) (make array-type #:shape shape #:value memory)))]
    (apply (lambda (type shape data size)
             (array (array-type type) shape (memory data size)))
           lst)))

(define (import-video-frame lst)
  (let [(memory (lambda (data size) (make <mem> #:base data #:size size)))] 
    (apply (lambda (format shape offsets pitches data size)
             (make <image>
                   #:format  (format->symbol format)
                   #:shape   shape
                   #:offsets offsets
                   #:pitches pitches
                   #:mem     (memory data size)))
           lst)))

(define (import-frame lst)
  ((case (car lst) ((audio) import-audio-frame) ((video) import-video-frame)) (cdr lst)))

(define (read-selected self audio video)
  (let [(frame (ffmpeg-read-audio/video (slot-ref self 'ffmpeg) audio video))]
    (and frame (import-frame frame))))

(define (read-audio self) (read-selected self #t #f))
(define (read-video self) (read-selected self #f #t))

(define (audio-pts self) (ffmpeg-audio-pts (slot-ref self 'ffmpeg)))
(define (video-pts self) (ffmpeg-video-pts (slot-ref self 'ffmpeg)))

(define-method (channels (self <ffmpeg>)) (ffmpeg-channels (slot-ref self 'ffmpeg)))
(define-method (rate (self <ffmpeg>)) (ffmpeg-rate (slot-ref self 'ffmpeg)))
(define-method (typecode (self <ffmpeg>))
  (audio-format->type (ffmpeg-typecode (slot-ref self 'ffmpeg))))
