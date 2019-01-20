(use-modules (oop goops) (aiscm core) (aiscm samples) (aiscm util))

(load-extension "libguile-aiscm-ffmpeg" "init_ffmpeg")

(define-class* <ffmpeg> <object> <meta<ffmpeg>> <class>
               (ffmpeg #:init-keyword #:ffmpeg)
               (video-buffer #:init-value '())
               (is-input #:init-keyword #:is-input #:getter is-input?)
               (audio-pts #:init-value 0 #:getter audio-pts)
               (video-pts #:init-value 0 #:getter video-pts))

(define (open-ffmpeg-input file-name)
  "Open audio/video input file FILE-NAME using FFmpeg library"
  (let [(debug (equal? "YES" (getenv "DEBUG")))]
    (make <ffmpeg> #:ffmpeg (make-ffmpeg-input file-name debug) #:is-input #t)))

(define self (open-ffmpeg-input "tests/fixtures/cat.wav"))
;(define self (open-ffmpeg-input "tests/fixtures/test.mp3"))

(define (make-audio-frame sample-format shape rate offsets data size)
  "Construct an audio frame from the specified information"
  (make <samples> #:typecode (sample-format->type sample-format)
                  #:shape    shape
                  #:rate     rate
                  #:offsets  offsets
                  #:planar   (sample-format->planar sample-format)
                  #:memory   data))

(define (target-audio-frame self)
  "Get target audio frame for audio encoding"
  (apply make-audio-frame (ffmpeg-target-audio-frame (slot-ref self 'ffmpeg))))

(define inverse-typemap
  (list (cons AV_SAMPLE_FMT_U8   <ubyte> )
        (cons AV_SAMPLE_FMT_U8P  <ubyte> )
        (cons AV_SAMPLE_FMT_S16  <sint>  )
        (cons AV_SAMPLE_FMT_S16P <sint>  )
        (cons AV_SAMPLE_FMT_S32  <int>   )
        (cons AV_SAMPLE_FMT_S32P <int>   )
        (cons AV_SAMPLE_FMT_FLT  <float> )
        (cons AV_SAMPLE_FMT_FLTP <float> )
        (cons AV_SAMPLE_FMT_DBL  <double>)
        (cons AV_SAMPLE_FMT_DBLP <double>)))

(define-method (typecode (self <ffmpeg>))
  "Query audio type of file"
  (assq-ref inverse-typemap (ffmpeg-typecode (slot-ref self 'ffmpeg))))

(define-method (channels (self <ffmpeg>))
  "Query number of audio channels"
  (ffmpeg-channels (slot-ref self 'ffmpeg)))

(ffmpeg-decode-audio/video (slot-ref self 'ffmpeg))

(shape (target-audio-frame self))

(convert-samples (target-audio-frame self) (typecode self) #f)
