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
            make-ulong make-long))
(define signed 'signed)
(define unsigned 'unsigned)
(define-class <int<>> (<element>))
(define-generic bits)
(define-generic signed?)
(define (make-int-class nbits sgn)
  (let* ((name (format #f "<int<~a,~a>>" nbits sgn))
         (metaname (format #f "<meta<~a>" name))
         (metaclass (make-class (list <class>) '() #:name metaname))
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
