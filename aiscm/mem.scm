(define-module (aiscm mem)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 optargs)
  #:use-module (aiscm element)
  #:export (<mem>
            get-memory
            read-bytes
            write-bytes))
(load-extension "libguile-mem" "init_mem")
(define-class <mem> ()
  (memory #:init-keyword #:memory #:getter get-memory)
  (base #:init-keyword #:base)
  (size #:init-value 0 #:init-keyword #:size #:getter get-size))
(define-method (initialize (self <mem>) initargs)
  (let-keywords initargs #f (memory base size)
    (if (not (and memory base))
      (let [(ptr (gc-malloc-pointerless size))]
        (next-method self (list #:memory ptr #:base ptr #:size size)))
      (next-method))))
(define-generic +)
(define-method (+ (self <mem>) (offset <integer>))
  (let [(size (get-size self))]
    (cond
      ((< offset 0) (throw 'mem-plus-offset-lt-zero offset))
      ((> offset size) (throw 'mem-plus-offset-gt-size offset size))
      (else
        (make <mem>
          #:memory (make-pointer (+ offset (pointer-address (get-memory self))))
          #:base (slot-ref self 'base)
          #:size (- size offset))))))
(define-method (equal? (a <mem>) (b <mem>))
  (equal? (get-memory a) (get-memory b)))
(define-method (read-bytes (self <mem>) (size <integer>))
  (if
    (> size (get-size self))
    (throw 'mem-read-size-overrun size (get-size self))
    (pointer->bytevector (get-memory self) size)))
(define-method (write-bytes (self <mem>) (bv <bytevector>))
  (bytevector-copy!
    bv 0
    (pointer->bytevector (get-memory self) (get-size self)) 0
    (bytevector-length bv))
  bv)
(define-method (display (self <mem>) port)
  (format port "#<<mem> #x~x ~a>"
          (pointer-address (get-memory self)) (get-size self)))
(define-method (write (self <mem>) port)
  (format port "#<<mem> #x~x ~a>"
          (pointer-address (get-memory self)) (get-size self)))
