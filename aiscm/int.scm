(define-module (aiscm int)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (aiscm element)
  #:use-module (aiscm float)
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
(define-class* <int<>> <element> <meta<int<>>> <meta<element>>)
(define-method (write (self <int<>>) port)
  (format port "#<~a ~a>" (class-name (class-of self)) (get-value self)))
(define-generic signed?)
(define-generic bits)
(define (integer nbits sgn)
  (template-class (int nbits sgn) <int<>>
    (lambda (class metaclass)
      (define-method (bits (self metaclass)) nbits)
      (define-method (signed? (self metaclass)) (eq? sgn 'signed)))))
(define-method (size-of (self <meta<int<>>>))
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
(define-method (pack (self <int<>>))
  (let* [(typecode (class-of self))
         (retval   (make-bytevector (size-of typecode)))
         (setter   (if (signed? typecode) bytevector-sint-set! bytevector-uint-set!))]
    (setter retval 0 (get-value self) (native-endianness) (size-of typecode))
    retval))
(define-method (unpack (self <meta<int<>>>) (packed <bytevector>))
  (let* [(ref   (if (signed? self) bytevector-sint-ref bytevector-uint-ref))
         (value (ref packed 0 (native-endianness) (size-of self)))]
    (make self #:value value)))
(define-method (coerce (a <meta<int<>>>) (b <meta<int<>>>))
  (integer (max (bits a) (bits b)) (if (or (signed? a) (signed? b)) signed unsigned)))
(define-method (coerce (a <meta<int<>>>) (b <meta<float<>>>)) b)
(define-method (coerce (a <meta<float<>>>) (b <meta<int<>>>)) a)
(define-method (match (i <integer>) . args)
  (if (every integer? args)
    (let [(lower (apply min (cons i args)))
          (upper (apply max (cons i args)))]
      (if (>= lower 0)
        (cond ((< upper (ash 1  8)) <ubyte>)
              ((< upper (ash 1 16)) <usint>)
              ((< upper (ash 1 32)) <uint> )
              ((< upper (ash 1 64)) <ulong>)
              (else (next-method)))
        (let [(nlower (max (lognot lower) upper))]
          (cond ((< nlower (ash 1  7)) <byte>)
                ((< nlower (ash 1 15)) <sint>)
                ((< nlower (ash 1 31))  <int>)
                ((< nlower (ash 1 63)) <long>)
                (else (next-method))))))
    (next-method)))
(define-method (types (self <meta<int<>>>)) (list self))
(define-method (content (self <integer>)) (list self))
