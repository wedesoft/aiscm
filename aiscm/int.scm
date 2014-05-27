(define-module (aiscm int)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (aiscm element)
  #:use-module (aiscm util)
  #:export (signed
            unsigned
            bits
            signed?
            integer
            <int<>> <meta<int<>>>
            <ubyte> <int<8,unsigned>>  <meta<int<8,unsigned>>>
            <byte>  <int<8,signed>>    <meta<int<8,signed>>>
            <usint> <int<16,unsigned>> <meta<int<16,unsigned>>>
            <sint>  <int<16,signed>>   <meta<int<16,signed>>>
            <uint>  <int<32,unsigned>> <meta<int<32,unsigned>>>
            <int>   <int<32,signed>>   <meta<int<32,signed>>>
            <ulong> <int<64,unsigned>> <meta<int<64,unsigned>>>
            <long>  <int<64,signed>>   <meta<int<64,signed>>>
            <native-int>))
(define signed 'signed)
(define unsigned 'unsigned)
(define-class <meta<int<>>> (<meta<element>>))
(define-class <int<>> (<element>)
              #:metaclass <meta<int<>>>) (define-generic bits)
(define-generic signed?)
(define (integer nbits sgn)
  (let* [(name (format #f "<int<~a,~a>>" nbits sgn))
         (metaname (format #f "<meta~a>" name))
         (metaclass (def-once metaname (make <class>
                                             #:dsupers (list <meta<int<>>>)
                                             #:slots '()
                                             #:name metaname)))
         (retval (def-once name (make metaclass
                                      #:dsupers (list <int<>>)
                                      #:slots '()
                                      #:name name)))]
    (define-method (bits (self metaclass)) nbits)
    (define-method (signed? (self metaclass)) (eq? sgn 'signed))
    retval))
; make metaclass #:dsupers #:slots #:name options
(define-method (storage-size (self <meta<int<>>>))
  (quotient (+ (bits self) 7) 8))
(define native-bits (* (sizeof '*) 8))
(define <ubyte> (integer  8 unsigned))
(define <byte>  (integer  8 signed  ))
(define <usint> (integer 16 unsigned))
(define <sint>  (integer 16 signed  ))
(define <uint>  (integer 32 unsigned))
(define <int>   (integer 32 signed  ))
(define <ulong> (integer 64 unsigned))
(define <long>  (integer 64 signed  ))
(define <native-int> (integer native-bits signed))
(define-method (foreign-type (t  <meta<int<8,unsigned>>>))  uint8)
(define-method (foreign-type (t    <meta<int<8,signed>>>))   int8)
(define-method (foreign-type (t <meta<int<16,unsigned>>>)) uint16)
(define-method (foreign-type (t   <meta<int<16,signed>>>))  int16)
(define-method (foreign-type (t <meta<int<32,unsigned>>>)) uint32)
(define-method (foreign-type (t   <meta<int<32,signed>>>))  int32)
(define-method (foreign-type (t <meta<int<64,unsigned>>>)) uint64)
(define-method (foreign-type (t   <meta<int<64,signed>>>))  int64)
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
  (let [(value (u8-list->int (bytevector->u8-list packed)))]
    (if (and (signed? self) (>= value (expt 2 (- (bits self) 1))))
      (raw-negative (make self #:value value))
      (make self #:value value))))
(define-method (write (self <int<>>) port)
  (format port "#<~a ~a>" (class-name (class-of self)) (get-value self)))
(define-method (display (self <int<>>) port)
  (format port "#<~a ~a>" (class-name (class-of self)) (get-value self)))
(define-method (coerce (a <meta<int<>>>) (b <meta<int<>>>))
  (integer (max (bits a) (bits b)) (if (or (signed? a) (signed? b)) signed unsigned)))
(define-method (match (i <integer>))
  (if (>= i 0)
    (cond ((< i (ash 1  8)) <ubyte>)
          ((< i (ash 1 16)) <usint>)
          ((< i (ash 1 32)) <uint> )
          ((< i (ash 1 64)) <ulong>)
          (else (next-method)))
    (let [(ni (lognot i))]
      (cond ((< ni (ash 1  7)) <byte>)
            ((< ni (ash 1 15)) <sint>)
            ((< ni (ash 1 31))  <int>)
            ((< ni (ash 1 63)) <long>)
            (else (next-method))))))
