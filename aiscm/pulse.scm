(define-module (aiscm pulse)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm mem)
  #:use-module (aiscm op)
  #:use-module (ice-9 optargs)
  #:use-module (aiscm util)
  #:export (<pulse> <meta<pulse>>
            write-samples))
(load-extension "libguile-pulse" "init_pulse")
(define-class* <pulse> <object> <meta<pulse>> <class>
               (pulse #:init-keyword #:pulse))
(define-method (initialize (self <pulse>) initargs)
  (let-keywords initargs #f (rate channels)
    (let [(rate     (or rate 44100))
          (channels (or channels 2))]
      (next-method self (list #:pulse (make-pulsedev rate channels))))))
(define-method (destroy (self <pulse>)) (pulsedev-destroy (slot-ref self 'pulse)))
(define (write-samples samples self)
  (let* [(n         (* (size samples) (size-of (typecode samples))))
         (compacted (ensure-default-strides samples))
         (data      (get-memory (slot-ref compacted 'value)))]
    (pulsedev-write (slot-ref self 'pulse) data n)))
