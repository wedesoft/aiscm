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
(define-module (aiscm pulse)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (aiscm core)
  #:use-module (aiscm samples)
  #:use-module (aiscm util)
  #:export (<pulse> <meta<pulse>>
            <pulse-play> <meta<pulse-play>>
            <pulse-record> <meta<pulse-record>>
            PA_SAMPLE_U8 PA_SAMPLE_S16LE PA_SAMPLE_S32LE PA_SAMPLE_FLOAT32LE
            type->pulse-type pulse-type->type flush drain latency))

(load-extension "libguile-aiscm-pulse" "init_pulse")

(define-class* <pulse> <object> <meta<pulse>> <class>
               (pulsedev #:init-keyword #:pulsedev                  )
               (channels #:init-keyword #:channels #:getter channels)
               (rate     #:init-keyword #:rate     #:getter rate    )
               (typecode #:init-keyword #:typecode #:getter typecode))
(define-method (initialize (self <pulse>) initargs)
  (let-keywords initargs #f (device typecode channels rate latency playback)
    (let* [(pulse-type (type->pulse-type (or typecode <sint>)))
           (channels   (or channels 2))
           (rate       (or rate 44100))
           (latency    (or latency 0.2))
           (pulsedev   (make-pulsedev device pulse-type playback channels rate latency))]
      (next-method self (list #:pulsedev pulsedev #:channels channels #:rate rate #:typecode typecode)))))

(define-class* <pulse-play> <pulse> <meta<pulse-play>> <meta<pulse>>)
(define-method (initialize (self <pulse-play>) initargs)
  (next-method self (append initargs (list #:playback #t))))
(define-class* <pulse-record> <pulse> <meta<pulse-record>> <meta<pulse>>)
(define-method (initialize (self <pulse-record>) initargs)
  (next-method self (append initargs (list #:playback #f))))

(define typemap
  (list (cons <ubyte> PA_SAMPLE_U8)
        (cons <sint>  PA_SAMPLE_S16LE)
        (cons <int>   PA_SAMPLE_S32LE)
        (cons <float> PA_SAMPLE_FLOAT32LE)))
(define inverse-typemap (alist-invert typemap))
(define (type->pulse-type type)
  "convert type class to Pulse audio type tag"
  (or (assq-ref typemap type) (aiscm-error 'type->pulse-type "Type ~a not supported by Pulse audio" type)))
(define (pulse-type->type pulse-type)
  "convert Pulse audio type tag to type class"
  (assq-ref inverse-typemap pulse-type))
(define-method (destroy (self <pulse>))
  (pulsedev-destroy (slot-ref self 'pulsedev)))

(define-method (write-audio (samples <multiarray<>>) (self <pulse-play>)); TODO: check type
  (pulsedev-write (slot-ref self 'pulsedev)
                  (memory  (ensure-default-strides samples))
                  (apply * (size-of (typecode samples)) (shape samples)))
  samples)

(define-method (write-audio (samples <samples>) (self <pulse-play>)); TODO: check type
  (write-audio (to-array samples) self)
  samples)

(define-method (write-audio (samples <procedure>) (self <pulse-play>))
  (let [(result (samples))]
    (while result
      (write-audio result self)
      (set! result (samples)))))

(define-method (read-audio (self <pulse-record>) (count <integer>))
  (let* [(size    (* count (channels self) (size-of (typecode self))))
         (memory (pulsedev-read (slot-ref self 'pulsedev) size))]
    (make (multiarray (typecode self) 2) #:shape (list (channels self) count) #:memory memory)))

(define (flush self) (pulsedev-flush (slot-ref self 'pulsedev)))
(define (drain self) (pulsedev-drain (slot-ref self 'pulsedev)))
(define (latency self) (pulsedev-latency (slot-ref self 'pulsedev)))
