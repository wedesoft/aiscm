(define-module (aiscm mem)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (aiscm element)
  #:use-module (aiscm util)
  #:export (<mem>
            get-memory
            read-bytes
            write-bytes))
(define-class <mem> ()
  (memory #:init-keyword #:memory #:getter get-memory)
  (base #:init-keyword #:base)
  (size #:init-value 0 #:init-keyword #:size #:getter get-size))
(define-method (initialize (self <mem>) initargs)
  (let-keywords initargs #f (memory base size)
    (if base
      (next-method self (list #:memory (or memory base) #:base base #:size size))
      (let [(ptr (gc-malloc-pointerless size))]
        (next-method self (list #:memory ptr #:base ptr #:size size))))))
(define-generic +)
(define-method (+ (self <mem>) (offset <integer>))
  (let [(size (get-size self))]
    (if (negative? offset)
      (scm-error 'misc-error
                 '+
                 "Offset not be lower than zero but was ~a"
                 (list offset)
                 #f)
      (make <mem>
            #:memory (make-pointer (+ offset (pointer-address (get-memory self))))
            #:base (slot-ref self 'base)
            #:size (- size offset)))))
(define-method (equal? (a <mem>) (b <mem>))
  (equal? (get-memory a) (get-memory b)))
(define-method (read-bytes (self <mem>) (size <integer>))
  (if (> size (get-size self))
      (scm-error 'misc-error
                 'read-bytes
                 "Attempt to read ~a bytes from memory of size ~a"
                 (list size (get-size self))
                 #f)
      (pointer->bytevector (get-memory self) size)))
(define-method (write-bytes (self <mem>) (bv <bytevector>))
  (bytevector-copy!
    bv 0
    (pointer->bytevector (get-memory self) (get-size self)) 0
    (bytevector-length bv)))
(define-method (write (self <mem>) port)
  (format port "#<<mem> #x~x ~a>"
          (pointer-address (get-memory self)) (get-size self)))
