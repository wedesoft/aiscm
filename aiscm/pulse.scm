(define-module (aiscm pulse)
  #:use-module (ice-9 threads)
  #:use-module (oop goops)
  #:use-module (aiscm int)
  #:use-module (aiscm float)
  #:use-module (aiscm util)
  #:export (<pulse> <meta<pulse>>
            PA_SAMPLE_U8 PA_SAMPLE_S16LE PA_SAMPLE_S32LE PA_SAMPLE_FLOAT32LE
            type->pulse-type pulse-type->type
            pulsedev-mainloop-run pulsedev-mainloop-quit))
(load-extension "libguile-aiscm-pulse" "init_pulse")
(define-class* <pulse> <object> <meta<pulse>> <class>
               (pulsedev #:init-keyword #:pulsedev)
               (thread #:init-keyword #:thread))
(define-method (initialize (self <pulse>) initargs)
  (let* [(pulsedev (make-pulsedev))
         (thread   (make-thread (lambda _ (pulsedev-mainloop-run pulsedev))))]
  (next-method self (list #:pulsedev pulsedev #:thread thread))))
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
  (pulsedev-mainloop-quit (slot-ref self 'pulsedev) 0)
  (join-thread (slot-ref self 'thread))
  (pulsedev-destroy (slot-ref self 'pulsedev)))
