(define-module (aiscm pulse)
  #:use-module (oop goops)
  #:use-module (aiscm int)
  #:use-module (aiscm float)
  #:use-module (aiscm util)
  #:export (<pulse> <meta<pulse>>
            PA_SAMPLE_U8 PA_SAMPLE_S16LE PA_SAMPLE_S32LE PA_SAMPLE_FLOAT32LE
            type->pulse-type pulse-type->type
            pulsedev-mainloop-run pulsedev-mainloop-quit pulsedev-destroy))
(load-extension "libguile-aiscm-pulse" "init_pulse")
(define-class* <pulse> <object> <meta<pulse>> <class>
               (pulse #:init-keyword #:pulse))
(define-method (initialize (self <pulse>) initargs)
  (next-method self (list #:pulse (make-pulsedev))))
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
