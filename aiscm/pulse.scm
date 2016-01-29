(define-module (aiscm pulse)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm mem)
  #:use-module (aiscm op)
  #:use-module (ice-9 optargs)
  #:use-module (aiscm util)
  #:export (<pulse> <meta<pulse>>
            rate channels
            write-samples latency drain
            check-audio-sample-shape))
(load-extension "libguile-pulse" "init_pulse")
(define-class* <pulse> <object> <meta<pulse>> <class>
               (pulse    #:init-keyword #:pulse)
               (rate     #:init-keyword #:rate     #:getter rate)
               (channels #:init-keyword #:channels #:getter channels))
(define-method (initialize (self <pulse>) initargs)
  (let-keywords initargs #f (rate channels)
    (let [(rate     (or rate 44100))
          (channels (or channels 2))]
      (next-method self (list #:pulse (make-pulsedev rate channels)
                              #:rate rate
                              #:channels channels)))))
(define-method (destroy (self <pulse>)) (pulsedev-destroy (slot-ref self 'pulse)))
(define (check-audio-sample-shape shape channels)
  (case (length shape)
    ((1) (if (not (eqv? 1 channels))
             (scm-error 'misc-error
                         'write-samples
                         "Audio sample array is one dimensional but audio output has ~a channels"
                         (list channels)
                         #f)))
    ((2) (if (not (eqv? (car shape) channels))
             (scm-error 'misc-error
                        'write-samples
                        "The first dimension of the sample array must match the number of channels ~a (but was ~a)"
                        (list channels (car shape))
                        #f)))
    (else (scm-error 'misc-error
                     'write-samples
                     "Audio sample array must not have more than 2 dimensions (but it had ~a)"
                     (list (length shape))
                     #f))))
(define (write-samples samples self)
  (if (not (eq? (typecode samples) <sint>))
    (scm-error 'misc-error
               'write-samples
               "Audio samples need to consist of short integers (but was ~a)"
               (list (typecode samples))
               #f))
  (check-audio-sample-shape (shape samples) (channels self))
  (pulsedev-write (slot-ref self 'pulse)
                  (get-memory (slot-ref (ensure-default-strides samples) 'value))
                  (size-of samples))
  samples)
(define (latency self) (* 1e-6 (pulsedev-latency (slot-ref self 'pulse))))
(define (drain self) (pulsedev-drain (slot-ref self 'pulse)) self)
