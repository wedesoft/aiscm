(define-module (aiscm pulse)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (aiscm mem)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm float)
  #:use-module (aiscm sequence)
  #:use-module (aiscm jit)
  #:use-module (aiscm util)
  #:export (<pulse> <meta<pulse>>
            <pulse-play> <meta<pulse-play>>
            <pulse-record> <meta<pulse-record>>
            PA_SAMPLE_U8 PA_SAMPLE_S16LE PA_SAMPLE_S32LE PA_SAMPLE_FLOAT32LE
            type->pulse-type pulse-type->type flush drain latency)
  #:re-export (destroy read-audio write-audio channels rate typecode))

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
  (or (assq-ref typemap type) (aiscm-error 'type->pulse-type "Type ~a not supported by Pulse audio" type)))
(define (pulse-type->type pulse-type)
  (assq-ref inverse-typemap pulse-type))
(define-method (destroy (self <pulse>))
  (pulsedev-destroy (slot-ref self 'pulsedev)))

(define-method (write-audio (samples <sequence<>>) (self <pulse-play>)); TODO: check type
  (pulsedev-write (slot-ref self 'pulsedev) (get-memory (value (ensure-default-strides samples))) (size-of samples)))
(define-method (write-audio (samples <procedure>) (self <pulse-play>))
  (let [(result (samples))]
    (while result
      (write-audio result self)
      (set! result (samples)))))
(define-method (read-audio (self <pulse-record>) (count <integer>))
  (let* [(size    (* count (channels self) (size-of (typecode self))))
         (samples (pulsedev-read (slot-ref self 'pulsedev) size))
         (memory  (make <mem> #:base samples #:size size))]
    (make (multiarray (typecode self) 2) #:shape (list (channels self) count) #:value memory)))

(define (flush self) (pulsedev-flush (slot-ref self 'pulsedev)))
(define (drain self) (pulsedev-drain (slot-ref self 'pulsedev)))
(define (latency self) (pulsedev-latency (slot-ref self 'pulsedev)))
