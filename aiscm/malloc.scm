(define-module (aiscm malloc)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (make-malloc
            get-memory
            get-size
            read-bytes
            write-bytes))
(load-extension "libguile-malloc" "init_malloc")
(define-class <malloc> ()
  (memory #:init-keyword #:memory #:getter get-memory)
  (base #:init-keyword #:base)
  (size #:init-value 0 #:init-keyword #:size #:getter get-size))
(define (make-malloc size)
  (let ((ptr (gc-malloc-pointerless size)))
    (make <malloc> #:memory ptr #:base ptr #:size size)))
(define-method (+ (self <malloc>) (offset <integer>))
  (let ((size (get-size self)))
    (cond
      ((< offset 0) (throw 'malloc-plus-offset-lt-zero offset))
      ((> offset size) (throw 'malloc-plus-offset-gt-size offset size))
      (else
        (make <malloc>
          #:memory (make-pointer (+ offset (pointer-address (get-memory self))))
          #:base (slot-ref self 'base)
          #:size (- size offset))))))
(define-method (read-bytes (self <malloc>) (size <integer>))
  (if
    (> size (get-size self))
    (throw 'malloc-read-size-overrun size (get-size self))
    (pointer->bytevector (get-memory self) size)))
(define-method (write-bytes (self <malloc>) (bv <bytevector>))
  (begin
    (bytevector-copy!
      bv 0
      (pointer->bytevector (get-memory self) (get-size self)) 0
      (bytevector-length bv))
    bv))
