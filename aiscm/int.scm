(define-module (aiscm int)
  #:use-module (aiscm element)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:export (signed
            unsigned
            bits
            signed?
            integer
            <ubyte> <byte>
            <usint> <sint>
            <uint> <int>
            <ulong> <long>))
(define signed 'signed)
(define unsigned 'unsigned)
(define-class <meta<int<>>> (<class>))
(define-class <int<>> (<element>) #:metaclass <meta<int<>>>)
(define-generic bits)
(define-generic signed?)
(define (integer nbits sgn)
  (let* ((name (format #f "<int<~a,~a>>" nbits sgn))
         (metaname (format #f "<meta~a>" name))
         (metaclass (make <class>
                          #:dsupers (list <meta<int<>>>)
                          #:slots '()
                          #:name metaname))
         (retval (make metaclass
                       #:dsupers (list <int<>>)
                       #:slots '()
                       #:name name)))
    (define-method (bits (self metaclass)) nbits)
    (define-method (signed? (self metaclass)) (eq? sgn 'signed))
    retval))
; make metaclass #:dsupers #:slots #:name options
(define-method (storage-size (self <meta<int<>>>))
  (quotient (+ (bits self) 7) 8))
(define <ubyte> (integer  8 unsigned))
(define <byte>  (integer  8 signed  ))
(define <usint> (integer 16 unsigned))
(define <sint>  (integer 16 signed  ))
(define <uint>  (integer 32 unsigned))
(define <int>   (integer 32 signed  ))
(define <ulong> (integer 64 unsigned))
(define <long>  (integer 64 signed  ))
(define (int->u8-list i n)
  (if (> n 0)
    (cons (logand #xff i) (int->u8-list (ash i -8) (- n 1)))
    '()))
(define (u8-list->int lst)
  (if (null? lst)
    #x00
    (logior (car lst) (ash (u8-list->int (cdr lst)) 8))))
(define-method (raw-negative (self <int<>>))
  (make (class-of self)
        #:value (- (get-value self) (expt 2 (bits (class-of self))))))
(define-method (pack (self <int<>>))
  (u8-list->bytevector
    (int->u8-list
      (get-value self)
      (storage-size (class-of self)))))
(define-method (unpack (self <meta<int<>>>) (packed <bytevector>))
  (let ((value (u8-list->int (bytevector->u8-list packed))))
    (if (and (signed? self) (>= value (expt 2 (- (bits self) 1))))
      (raw-negative (make self #:value value))
      (make self #:value value))))
(define-method (write (self <int<>>) port)
  (format port "#<~a ~a>" (class-name (class-of self)) (get-value self)))
(define-method (display (self <int<>>) port)
  (format port "#<~a ~a>" (class-name (class-of self)) (get-value self)))
