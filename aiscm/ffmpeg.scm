;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 Jan Wedekind <jan@wedesoft.de>
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
  #:use-module (aiscm core)
  #:use-module (aiscm samples)
  #:use-module (aiscm image)
  #:use-module (aiscm util)
  #:export (<ffmpeg>
            open-ffmpeg-input open-ffmpeg-output is-input? frame-rate video-pts audio-pts pts=
            video-bit-rate aspect-ratio video-buffer-push video-buffer-pop select-rate target-video-frame
            select-sample-typecode typecodes-of-sample-formats best-sample-format select-sample-format
            target-audio-frame packed-audio-frame audio-buffer-fill video-buffer-fill have-audio? have-video?
            buffer-timestamped-video buffer-timestamped-audio buffer-audio fetch-audio decode-audio/video
            crop-audio-frame-size))


(load-extension "libguile-aiscm-ffmpeg" "init_ffmpeg")

(define-class* <ffmpeg> <object> <meta<ffmpeg>> <class>
               (ffmpeg #:init-keyword #:ffmpeg)
               (video-buffer #:init-value '())
               (is-input #:init-keyword #:is-input #:getter is-input?)
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
    (make <ffmpeg> #:ffmpeg (make-ffmpeg-input file-name debug) #:is-input #t)))

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
           (shape          (or shape '(288 384)))
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
                                         debug)
            #:is-input #f))))

(define-method (destroy (self <ffmpeg>))
  "Destructor"
  (if (and (not (is-input? self)) (have-audio? self))
    (begin
      (crop-audio-frame-size self (/ (audio-buffer-fill self) (sample-size self)))
      (encode-audio self)))
  (ffmpeg-destroy (slot-ref self 'ffmpeg)))

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

(define (make-video-frame format shape offsets pitches data size)
  "Construct a video frame from the specified information"
  (make <image>
        #:format  (format->symbol format)
        #:shape   shape
        #:offsets offsets
        #:pitches pitches
        #:memory  data))

(define (make-audio-frame sample-format shape rate offsets data size)
  "Construct an audio frame from the specified information"
  (make <samples> #:typecode (sample-format->type sample-format)
                  #:shape    shape
                  #:rate     rate
                  #:offsets  offsets
                  #:planar   (sample-format->planar sample-format)
                  #:memory   data))

(define (video-buffer-push self pts-and-frame)
  "Store frame and time stamp in the specified buffer"
  (slot-set! self 'video-buffer (attach (slot-ref self 'video-buffer) pts-and-frame)) #t)

(define (video-buffer-pop self)
  "Retrieve frame and timestamp from the specified buffer"
  (let [(lst (slot-ref self 'video-buffer))]
    (and
      (not (null? lst))
      (begin
        (slot-set! self 'video-pts    (caar lst))
        (slot-set! self 'video-buffer (cdr  lst))
        (cdar lst)))))

(define (decode-audio/video self)
  "Decode audio/video frames"
  (ffmpeg-decode-audio/video (slot-ref self 'ffmpeg)))

(define (buffer-audio/video self)
  "Decode and buffer audio/video frames"
  (let [(info (decode-audio/video self))]
     (and
       info
       (case (car info)
         ((audio) (buffer-timestamped-audio (cadr info) self))
         ((video) (buffer-timestamped-video (cadr info) self))))))

(define (sample-size self)
  "Get size of audio sample in bytes"
  (* (size-of (typecode self)) (channels self)))

(define-method (read-audio (self <ffmpeg>) (count <integer>))
  "Retrieve audio samples from input audio stream"
  (and
    (have-audio? self)
    (begin
      (while (< (audio-buffer-fill self) (* count (sample-size self))) (or (buffer-audio/video self) (break)))
      (and (not (zero? (audio-buffer-fill self)))
        (let* [(actual (min count (/ (audio-buffer-fill self) (sample-size self))))
               (result (make <samples> #:typecode (typecode self)
                                       #:shape (list actual (channels self))
                                       #:rate (rate self)
                                       #:planar #f))]
          (fetch-audio self result)
          result)))))

(define-method (read-image (self <ffmpeg>))
  "Retrieve the next video frame"
  (and (have-video? self)
       (or (video-buffer-pop self)
           (and (buffer-audio/video self) (read-image self)))))

(define (target-audio-frame self)
  "Get target audio frame for audio encoding"
  (apply make-audio-frame (ffmpeg-target-audio-frame (slot-ref self 'ffmpeg))))

(define (packed-audio-frame self)
  "Get packed audio frame for converting from/to audio buffer data"
  (apply make-audio-frame (ffmpeg-packed-audio-frame (slot-ref self 'ffmpeg))))

(define (video-buffer-fill self)
  "Get number of frames in video buffer"
  (length (slot-ref self 'video-buffer)))

(define (have-audio? self)
  "Check whether the file has an audio stream"
  (ffmpeg-have-audio? (slot-ref self 'ffmpeg)))

(define (have-video? self)
   "Check whether the file has a video stream"
   (ffmpeg-have-video? (slot-ref self 'ffmpeg)))

(define (audio-buffer-fill self)
  "Get number of bytes available in audio buffer"
  (ffmpeg-audio-buffer-fill (slot-ref self 'ffmpeg)))

(define (buffer-timestamped-video timestamp self)
  "Buffer a video frame"
  (video-buffer-push self (cons timestamp (duplicate (target-video-frame self)))))

(define (buffer-timestamped-audio timestamp self)
  "Buffer an audio frame"
  (buffer-audio (convert-samples (target-audio-frame self) (typecode self) #f) self))

(define (buffer-audio samples self)
  "Append audio data to audio buffer"
  (if (not (eqv? (channels self) (channels samples)))
    (aiscm-error 'buffer-audio "Sample need to have ~a channels but had ~a" (channels self) (channels samples)))
  (if (not (eq? (typecode self) (typecode samples)))
    (aiscm-error 'buffer-audio "Expected samples of type ~a but got samples of type ~a" (typecode self) (typecode samples)))
  (if (not (eqv? (rate self) (rate samples)))
    (aiscm-error 'buffer-audio "Samples need to have a rate of ~a Hz but had ~a Hz" (rate self) (rate samples)))
  (ffmpeg-buffer-audio (slot-ref self 'ffmpeg) (memory samples) (size-of samples)))

(define (fetch-audio self samples); TODO: fill target frame with desired number of packed samples
  "Fetch data from the audio buffer and put it into the samples"
  (ffmpeg-fetch-audio (slot-ref self 'ffmpeg) (memory samples) (size-of samples))
  (slot-set! self 'audio-pts (+ (slot-ref self 'audio-pts) (/ (car (shape samples)) (rate self)))))

(define (encode-audio self)
  "Encode buffered audio frames"
  (let* [(packed     (packed-audio-frame self))
         (target     (target-audio-frame self))]
    (while (>= (audio-buffer-fill self) (size-of packed))
      (fetch-audio self packed)
      (convert-samples-from! target packed)
      (ffmpeg-encode-audio (slot-ref self 'ffmpeg)))))

(define-method (write-audio (samples <samples>) (self <ffmpeg>))
  "Write audio frame to output stream"
  (buffer-audio samples self)
  (encode-audio self)
  samples)

(define-method (write-audio (samples <multiarray<>>) (self <ffmpeg>))
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

(define-method (write-image (img <multiarray<>>) (self <ffmpeg>))
  "Write array representing video frame to output video"
  (write-image (to-image img) self)
  img)

(define (pts= self position)
  "Set audio/video position (in seconds)"
  (ffmpeg-seek (slot-ref self 'ffmpeg) position)
  (ffmpeg-flush (slot-ref self 'ffmpeg))
  (slot-set! self 'video-buffer '())
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

(define (crop-audio-frame-size self size)
  "Crop encoder audio frames to size of final audio frame (specified size)"
  (if (not (zero? size)) (ffmpeg-crop-audio-frame-size (slot-ref self 'ffmpeg) size)))
