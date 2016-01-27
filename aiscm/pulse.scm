(define-module (aiscm pulse)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (aiscm util)
  #:export (<pulse> <meta<pulse>>))
(load-extension "libguile-pulse" "init_pulse")
(define-class* <pulse> <object> <meta<pulse>> <class>
               (pulse #:init-keyword #:pulse))
(define-method (initialize (self <pulse>) initargs)
  (let-keywords initargs #f (rate channels)
    (let [(rate     (or rate 44100))
          (channels (or channels 2))]
      (next-method self (list #:pulse (make-pulsedev rate channels))))))
(define-method (destroy (self <pulse>)) (pulsedev-destroy (slot-ref self 'pulse)))
