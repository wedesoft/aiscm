;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Wedekind <jan@wedesoft.de>
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
(define-module (aiscm ffmpeg)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (aiscm mem)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm sequence)
  #:use-module (aiscm float)
  #:use-module (aiscm mem)
  #:use-module (aiscm samples)
  #:use-module (aiscm image)
  #:use-module (aiscm jit)
  #:use-module (aiscm util)
  #:export (<ffmpeg>
            open-ffmpeg-input open-ffmpeg-output frame-rate video-pts audio-pts pts=
            video-bit-rate aspect-ratio ffmpeg-buffer-push ffmpeg-buffer-pop select-rate target-video-frame
            select-sample-typecode typecodes-of-sample-formats best-sample-format select-sample-format
            target-audio-frame packed-audio-frame audio-buffer-fill buffer-audio fetch-audio)
  #:re-export (destroy read-image write-image read-audio write-audio rate channels typecode))


(load-extension "libguile-aiscm-ffmpeg" "init_ffmpeg")

(define-class* <ffmpeg> <object> <meta<ffmpeg>> <class>
               (ffmpeg #:init-keyword #:ffmpeg)
               (audio-buffer #:init-value '())
               (video-buffer #:init-value '())
               (audio-pts #:init-value 0 #:getter audio-pts)
               (video-pts #:init-value 0 #:getter video-pts))

(define typemap
  (list (cons <ubyte>  AV_SAMPLE_FMT_U8P )
        (cons <sint>   AV_SAMPLE_FMT_S16P)
        (cons <int>    AV_SAMPLE_FMT_S32P)
        (cons <float>  AV_SAMPLE_FMT_FLTP)
        (cons <double> AV_SAMPLE_FMT_DBLP)))
(define inverse-typemap (alist-invert typemap))

(define (open-ffmpeg-input file-name)
  "Open audio/video input file FILE-NAME using FFmpeg library"
  (let [(debug (equal? "YES" (getenv "DEBUG")))]
    (make <ffmpeg> #:ffmpeg (make-ffmpeg-input file-name debug))))

(define (select-rate rate)
  "Check that the sample rate is supported or raise an exception"
  (if (number? rate)
    (lambda (rates)
      (if (or (null? rates) (memv rate rates))
        rate
        (aiscm-error 'select-rate "Sampling rate ~a not supported (supported rates are ~a)" rate rates)))
    rate))

(define (select-sample-typecode typecode supported-types)
  "Select the internal sample type for the FFmpeg encoder buffer.
  The type needs to be the same or larger than the type of the audio samples provided."
  (let* [(rank           (lambda (t) (+ (size-of t) (if (is-a? t <meta<float<>>>) 1 0))))
         (sufficient?    (lambda (t) (<= (rank typecode) (rank t))))
         (relevant-types (filter sufficient? (sort-by supported-types rank)))]
    (if (null? supported-types)
      typecode
      (if (null? relevant-types)
        (aiscm-error 'select-sample-typecode "Sample type ~a or larger type is not supported" typecode)
        (car relevant-types)))))

(define (typecodes-of-sample-formats sample-formats)
  "Get typecodes for a list of supported sample types"
  (delete-duplicates (map sample-format->type sample-formats)))

(define (best-sample-format selected-type sample-formats)
  "Select sample format with spcecified typecode. Prefer packed format over planar format."
  (let [(packed (type+planar->sample-format selected-type #f))
        (planar (type+planar->sample-format selected-type #t))]
    (if (memv packed sample-formats) packed planar)))

(define* ((select-sample-format typecode) supported-formats)
  "Select most suitable sample format for audio encoding"
  (best-sample-format (select-sample-typecode typecode (typecodes-of-sample-formats supported-formats)) supported-formats))

(define (open-ffmpeg-output file-name . initargs)
  "Open audio/video output file FILE-NAME using FFmpeg library"
  (let-keywords initargs #f (format-name
                             shape frame-rate video-bit-rate aspect-ratio
                             channels rate typecode audio-bit-rate)
    (let* [(have-audio     (or rate channels audio-bit-rate))
           (have-video     (or (not have-audio) shape frame-rate video-bit-rate aspect-ratio))
           (shape          (or shape '(384 288)))
           (frame-rate     (or frame-rate 25))
           (video-bit-rate (or video-bit-rate (apply * 3 shape)))
           (aspect-ratio   (or aspect-ratio 1))
           (select-rate    (select-rate (or rate 44100)))
           (typecode       (or typecode <float>))
           (sample-format  (type+planar->sample-format typecode #t))
           (select-format  (select-sample-format typecode))
           (channels       (or channels 2))
           (audio-bit-rate (or audio-bit-rate (round (/ (* 3 44100) 2))))
           (debug          (equal? "YES" (getenv "DEBUG")))]
      (make <ffmpeg>
            #:ffmpeg (make-ffmpeg-output file-name format-name
                                         (list shape frame-rate video-bit-rate aspect-ratio) have-video
                                         (list select-rate channels audio-bit-rate select-format) have-audio
                                         debug)))))

(define-method (destroy (self <ffmpeg>)) (ffmpeg-destroy (slot-ref self 'ffmpeg)))

(define-method (shape (self <ffmpeg>))
  "Get two-dimensional shape of video frames"
  (ffmpeg-shape (slot-ref self 'ffmpeg)))

(define (frame-rate self)
  "Query (average) frame rate of video"
  (ffmpeg-frame-rate (slot-ref self 'ffmpeg)))

(define (video-bit-rate self)
  "Query bit rate of video stream"
  (ffmpeg-video-bit-rate (slot-ref self 'ffmpeg)))

(define (aspect-ratio self)
  "Query pixel aspect ratio of video"
  (let [(ratio (ffmpeg-aspect-ratio (slot-ref self 'ffmpeg)))]
    (if (zero? ratio) 1 ratio)))

(define (import-audio-frame self lst)
  "Compose audio frame from timestamp, type, shape, data pointer, and size"
  (let [(memory     (lambda (data size) (make <mem> #:base data #:size size #:pointerless #t)))
        (array-type (lambda (type) (multiarray (assq-ref inverse-typemap type) 2)))
        (array      (lambda (array-type shape memory) (make array-type #:shape shape #:value memory)))]
    (apply (lambda (pts type shape data size)
                   (cons pts (array (array-type type) shape (memory data size))))
           lst)))

(define (make-video-frame format shape offsets pitches data size)
  "Construct a video frame from the specified information"
  (let [(memory (lambda (data size) (make <mem> #:base data #:size size #:pointerless #t)))]
    (make <image>
          #:format  (format->symbol format)
          #:shape   shape
          #:offsets offsets
          #:pitches pitches
          #:mem     (memory data size))))

(define (make-audio-frame sample-format shape rate offsets data size)
  "Construct an audio frame from the specified information"
  (let [(memory (lambda (data size) (make <mem> #:base data #:size size #:pointerless #t)))]
    (make <samples> #:typecode (sample-format->type sample-format)
                    #:shape    shape
                    #:rate     rate
                    #:offsets  offsets
                    #:planar   (sample-format->planar sample-format)
                    #:mem      (memory data size))))

(define (import-video-frame self lst)
  "Compose video frame from timestamp, format, shape, offsets, pitches, data pointer, and size"
  (let [(pts    (car lst))
        (frame  (apply make-video-frame (cdr lst)))]
    (cons pts (duplicate frame))))

(define (ffmpeg-buffer-push self buffer pts-and-frame)
  "Store frame and time stamp in the specified buffer"
  (slot-set! self buffer (attach (slot-ref self buffer) pts-and-frame)) #t)

(define (ffmpeg-buffer-pop self buffer clock)
  "Retrieve frame and timestamp from the specified buffer"
  (let [(lst (slot-ref self buffer))]
    (and
      (not (null? lst))
      (begin
        (slot-set! self clock  (caar lst))
        (slot-set! self buffer (cdr lst))
        (cdar lst)))))

(define (buffer-audio/video self)
  "Decode and buffer audio/video frames"
  (let [(lst (ffmpeg-read-audio/video (slot-ref self 'ffmpeg)))]
    (if lst
       (case (car lst)
         ((audio) (ffmpeg-buffer-push self 'audio-buffer (import-audio-frame self (cdr lst))))
         ((video) (ffmpeg-buffer-push self 'video-buffer (import-video-frame self (cdr lst)))))
       #f)))

(define-method (read-audio (self <ffmpeg>) (count <integer>))
  "Retrieve audio samples from input audio stream"
  (make <samples> #:typecode (typecode self) #:shape (list (channels self) count) #:rate (rate self) #:planar #f))

(define-method (read-image (self <ffmpeg>))
  "Retrieve the next video frame"
  (or (ffmpeg-buffer-pop self 'video-buffer 'video-pts) (and (buffer-audio/video self) (read-image self))))

(define (target-audio-frame self)
  "Get target audio frame for audio encoding"
  (apply make-audio-frame (ffmpeg-target-audio-frame (slot-ref self 'ffmpeg))))

(define (packed-audio-frame self)
  "Get packed audio frame for converting from/to audio buffer data"
  (apply make-audio-frame (ffmpeg-packed-audio-frame (slot-ref self 'ffmpeg))))

(define (audio-buffer-fill self)
  "Get number of bytes available in audio buffer"
  (ffmpeg-audio-buffer-fill (slot-ref self 'ffmpeg)))

(define (buffer-audio samples self)
  "Append audio data to audio buffer"
  (if (not (eqv? (channels self) (channels samples)))
    (aiscm-error 'buffer-audio "Sample need to have ~a channels but had ~a" (channels self) (channels samples)))
  (if (not (eq? (typecode self) (typecode samples)))
    (aiscm-error 'buffer-audio "Expected samples of type ~a but got samples of type ~a" (typecode self) (typecode samples)))
  (if (not (eqv? (rate self) (rate samples)))
    (aiscm-error 'buffer-audio "Samples need to have a rate of ~a Hz but had ~a Hz" (rate self) (rate samples)))
  (ffmpeg-buffer-audio (slot-ref self 'ffmpeg) (get-memory (slot-ref samples 'mem)) (size-of samples)))

(define (fetch-audio self)
  "Fetch data from the audio buffer and put it into the samples"
  (ffmpeg-fetch-audio (slot-ref self 'ffmpeg)))

(define-method (write-audio (samples <samples>) (self <ffmpeg>))
  "Write audio frame to output stream"
  (buffer-audio samples self)
  (let* [(packed     (packed-audio-frame self))
         (target     (target-audio-frame self))]
    (while (>= (audio-buffer-fill self) (size-of packed))
      (fetch-audio self); fills "packed" audio frame with samples
      (convert-samples-from! target packed)
      (ffmpeg-encode-audio (slot-ref self 'ffmpeg))))
  samples)

(define-method (write-audio (samples <sequence<>>) (self <ffmpeg>))
  "Write audio data to output stream"
  (write-audio (to-samples samples (rate self)) self)
  samples)

(define (target-video-frame self)
  "Get target video frame for video encoding"
  (apply make-video-frame (ffmpeg-target-video-frame (slot-ref self 'ffmpeg))))

(define-method (write-image (img <image>) (self <ffmpeg>))
  "Write video frame to output file"
  (convert-image-from! (target-video-frame self) img)
  (ffmpeg-encode-video (slot-ref self 'ffmpeg))
  img)

(define-method (write-image (img <sequence<>>) (self <ffmpeg>))
  "Write array representing video frame to output video"
  (write-image (to-image img) self)
  img)

(define (pts= self position)
  "Set audio/video position (in seconds)"
  (ffmpeg-seek (slot-ref self 'ffmpeg) position)
  (ffmpeg-flush (slot-ref self 'ffmpeg))
  position)

(define-method (channels (self <ffmpeg>))
  "Query number of audio channels"
  (ffmpeg-channels (slot-ref self 'ffmpeg)))

(define-method (rate (self <ffmpeg>))
  "Get audio sampling rate of file"
  (ffmpeg-rate (slot-ref self 'ffmpeg)))

(define-method (typecode (self <ffmpeg>))
  "Query audio type of file"
  (assq-ref inverse-typemap (ffmpeg-typecode (slot-ref self 'ffmpeg))))
