(define-module (aiscm int)
  #:use-module (aiscm element)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:export (signed
            unsigned
            bits
            signed?
            make-int-class
            <ubyte> <byte>
            <usint> <sint>
            <uint> <int>
            <ulong> <long>
            make-ubyte make-byte
            make-usint make-sint
            make-uint make-int
            make-ulong make-long
            pack
            unpack))
(define signed 'signed)
(define unsigned 'unsigned)
(define-class <meta<int<>>> (<class>))
(define-class <int<>> (<element>) #:metaclass <meta<int<>>>)
(define-generic bits)
(define-generic signed?)
(define (make-int-class nbits sgn)
  (let* ((name (format #f "<int<~a,~a>>" nbits sgn))
         (metaname (format #f "<meta<~a>" name))
         (metaclass (make-class (list <meta<int<>>>) '() #:name metaname))
         (retval (make-class (list <int<>>)
                             '()
                             #:name name
                             #:metaclass metaclass)))
    (define-method (bits (self metaclass)) nbits)
    (define-method (signed? (self metaclass)) (eq? sgn 'signed))
    retval))
(define <ubyte> (make-int-class  8 unsigned))
(define <byte>  (make-int-class  8 signed  ))
(define <usint> (make-int-class 16 unsigned))
(define <sint>  (make-int-class 16 signed  ))
(define <uint>  (make-int-class 32 unsigned))
(define <int>   (make-int-class 32 signed  ))
(define <ulong> (make-int-class 64 unsigned))
(define <long>  (make-int-class 64 signed  ))
(define (make-ubyte value) (make <ubyte> #:value value))
(define (make-byte  value) (make <byte>  #:value value))
(define (make-usint value) (make <usint> #:value value))
(define (make-sint  value) (make <sint>  #:value value))
(define (make-uint  value) (make <uint>  #:value value))
(define (make-int   value) (make <int>   #:value value))
(define (make-ulong value) (make <ulong> #:value value))
(define (make-long  value) (make <long>  #:value value))
(define-method (equal? (a <int<>>) (b <int<>>))
  (equal? (slot-ref a 'value) (slot-ref b 'value)))
(define (int->u8-list i n)
  (if (> n 0)
    (cons (logand #xff i) (int->u8-list (ash i -8) (- n 1)))
    '()))
(define (u8-list->int lst)
  (if (null? lst)
    #x00
    (logior (car lst) (ash (u8-list->int (cdr lst)) 8))))
(define-method (two-complement (self <int<>>))
  (make (class-of self)
        #:value (- (slot-ref self 'value) (expt 2 (bits (class-of self))))))
(define-method (pack (self <int<>>))
  (u8-list->bytevector
    (int->u8-list
      (slot-ref self 'value)
      (quotient (+ (bits (class-of self)) 7) 8))))
(define-method (unpack (self <meta<int<>>>) (packed <bytevector>))
  (let ((value (u8-list->int (bytevector->u8-list packed))))
    (if (and (signed? self) (>= value (expt 2 (- (bits self) 1))))
      (two-complement (make self #:value value))
      (make self #:value value))))
