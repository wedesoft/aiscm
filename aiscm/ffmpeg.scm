(define-module (aiscm ffmpeg)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (aiscm mem)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm sequence)
  #:use-module (aiscm float)
  #:use-module (aiscm mem)
  #:use-module (aiscm image)
  #:use-module (aiscm util)
  #:export (<ffmpeg> open-ffmpeg-input read-video read-audio frame-rate video-pts audio-pts read-audio/video pts=))

(load-extension "libguile-aiscm-ffmpeg" "init_ffmpeg")

(define-class* <ffmpeg> <object> <meta<ffmpeg>> <class>
               (ffmpeg #:init-keyword #:ffmpeg)
               (audio-pts #:init-value 0 #:getter audio-pts)
               (video-pts #:init-value 0 #:getter video-pts))
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
(define (open-ffmpeg-input file-name) (open-input file-name))

(define-method (shape (self <ffmpeg>)) (ffmpeg-shape (slot-ref self 'ffmpeg)))
(define (frame-rate self) (ffmpeg-frame-rate (slot-ref self 'ffmpeg)))

(define (import-audio-frame self lst)
  (let [(memory     (lambda (data size) (make <mem> #:base data #:size size)))
        (array-type (lambda (type) (multiarray (audio-format->type type) 2)))
        (array      (lambda (array-type shape memory) (make array-type #:shape shape #:value memory)))]
    (apply (lambda (pts type shape data size)
             (slot-set! self 'audio-pts pts)
             (array (array-type type) shape (memory data size)))
           lst)))

(define (import-video-frame self lst)
  (let [(memory (lambda (data size) (make <mem> #:base data #:size size)))]
    (apply (lambda (pts format shape offsets pitches data size)
             (slot-set! self 'video-pts pts)
             (make <image>
                   #:format  (format->symbol format)
                   #:shape   shape
                   #:offsets offsets
                   #:pitches pitches
                   #:mem     (memory data size)))
           lst)))

(define (import-frame self lst)
  ((case (car lst) ((audio) import-audio-frame) ((video) import-video-frame)) self (cdr lst)))

(define (audio? frame) (and (is-a? frame <sequence<>>) frame))
(define (video? frame) (and (is-a? frame <image>     ) frame))

(define (read-selected self pred)
  (if (ffmpeg-buffer-frame (slot-ref self 'ffmpeg))
    (let [(frame (ffmpeg-read-audio/video (slot-ref self 'ffmpeg)))]
      (or (pred (import-frame self frame)) (read-selected self pred)))
    #f))

(define (read-audio self) (read-selected self audio?))
(define (read-video self) (read-selected self video?))
(define (read-audio/video self) (read-selected self identity))

(define (pts= self position)
  (ffmpeg-seek (slot-ref self 'ffmpeg) position)
  (ffmpeg-flush (slot-ref self 'ffmpeg))
  position)

(define-method (channels (self <ffmpeg>)) (ffmpeg-channels (slot-ref self 'ffmpeg)))
(define-method (rate (self <ffmpeg>)) (ffmpeg-rate (slot-ref self 'ffmpeg)))
(define-method (typecode (self <ffmpeg>))
  (audio-format->type (ffmpeg-typecode (slot-ref self 'ffmpeg))))
