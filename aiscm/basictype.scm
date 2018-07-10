;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Wedekind <jan@wedesoft.de>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
(define-module (aiscm basictype)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (aiscm util)
  #:export (get integer signed unsigned bits signed? coerce foreign-type
            floating-point single-precision double-precision double-precision?
            decompose-argument decompose-result decompose-type compose-value compose-values
            complex base size-of unpack-value native-type components constructor build
            pointer target llvmlist typecode dimension
            multiarray dimensions shape memory memory-base strides llvmarray
            <void> <meta<void>>
            <scalar> <meta<scalar>>
            <structure> <meta<structure>>
            <bool>  <meta<bool>>
            <int<>> <meta<int<>>>
            <ubyte> <meta<ubyte>> <int<8,unsigned>>  <meta<int<8,unsigned>>>
            <byte>  <meta<byte>>  <int<8,signed>>    <meta<int<8,signed>>>
            <usint> <meta<usint>> <int<16,unsigned>> <meta<int<16,unsigned>>>
            <sint>  <meta<sint>>  <int<16,signed>>   <meta<int<16,signed>>>
            <uint>  <meta<uint>>  <int<32,unsigned>> <meta<int<32,unsigned>>>
            <int>   <meta<int>>   <int<32,signed>>   <meta<int<32,signed>>>
            <ulong> <meta<ulong>> <int<64,unsigned>> <meta<int<64,unsigned>>>
            <long>  <meta<long>>  <int<64,signed>>   <meta<int<64,signed>>>
            <float<>> <meta<float<>>>
            <float>  <meta<float>>  <float<single>> <meta<float<single>>>
            <double> <meta<double>> <float<double>> <meta<float<double>>>
            <complex<>>       <meta<complex<>>>
            <complex<float>>  <meta<complex<float>>>  <complex<float<single>>> <meta<complex<float<single>>>>
            <complex<double>> <meta<complex<double>>> <complex<float<double>>> <meta<complex<float<double>>>>
            <pointer<>> <meta<pointer<>>>
            <multiarray<>> <meta<multiarray<>>> <llvmarray<>> <meta<llvmarray<>>>)
  #:export-syntax (define-structure)
  #:re-export (real-part imag-part))


(define signed   'signed)
(define unsigned 'unsigned)

(define bool (1+ int64)); int64 is last foreign type

(define-class* <void> <object> <meta<void>> <class>
               (value #:init-keyword #:value #:getter get))

(define-class* <scalar> <void> <meta<scalar>> <meta<void>>)

(define-class* <structure> <void> <meta<structure>> <meta<void>>)

(define-class* <bool> <scalar> <meta<bool>> <meta<scalar>>)

(define-class* <int<>> <scalar> <meta<int<>>> <meta<scalar>>)

(define-class* <float<>> <scalar> <meta<float<>>> <meta<scalar>>)

(define-class* <pointer<>> <scalar> <meta<pointer<>>> <meta<scalar>>)

(define-class* <llvmlist<>> <structure> <meta<llvmlist<>>> <meta<structure>>)

(define-syntax-rule (component-accessor type name index)
  "Define accessor to access component of a composite type"
  (define-method (name (self type))
    (make (list-ref (base (class-of self)) index)
          #:value (lambda (fun) (list-ref ((get self) fun) index)))))

(define-syntax component-accessors
  (lambda (x)
    (syntax-case x ()
      ((k class metaclass members ...)
       "Define accessor methods for individual components of a composite type"
       (let [(n (length (syntax->datum #'(members ...))))]
         #`(begin
             (define-method (components (self metaclass))
               "List component accessor methods of composite type"
               (list members ...))
             .
             #,(map (lambda (member-name index)
                      #`(component-accessor class
                                            #,(datum->syntax #'k member-name)
                                            #,(datum->syntax #'k index)))
                    (syntax->datum #'(members ...))
                    (iota n))))))))

(define-syntax define-structure
  (lambda (x)
    (syntax-case x ()
      ((k name construct (members ...))
        (let [(class       (string->symbol (format #f "<~a<>>" (syntax->datum #'name))))
              (metaclass   (string->symbol (format #f "<meta<~a<>>>" (syntax->datum #'name))))
              (n           (length (syntax->datum #'(members ...))))
              (header      (map (cut list <> '<meta<void>>) (syntax->datum #'(members ...))))]
          #`(begin
              (define-class* #,(datum->syntax #'k class) <structure> #,(datum->syntax #'k metaclass) <meta<structure>>)

              (define-method (constructor (type #,(datum->syntax #'k metaclass)))
                "Get constructor for composite type"
                construct)

              (define-method (build (type #,(datum->syntax #'k metaclass)))
                "Get method for composing value in compiled code"
                name)

              (define-method (name #,@(datum->syntax #'k header))
                "Instantiate a composite type using the type template"
                (template-class (name members ...) #,(datum->syntax #'k class)
                  (lambda (class metaclass)
                    (define-method (base (self metaclass)) (list members ...)))))

              (define-method (name (base-type <meta<void>>))
                "Instantiate a composite type using the type template"
                (template-class (name base-type) #,(datum->syntax #'k class)
                  (lambda (class metaclass)
                    (define-method (base (self metaclass)) base (make-list #,(datum->syntax #'k n) base-type)))))

              (define-method (foreign-type (type #,(datum->syntax #'k metaclass)))
                "Foreign type of template class is pointer"
                int64)

              (component-accessors #,(datum->syntax #'k class) #,(datum->syntax #'k metaclass) members ...)))))))

(define (integer nbits sgn)
  "Retrieve integer class with specified number of bits and sign"
  (template-class (int nbits sgn) <int<>>
    (lambda (class metaclass)
      (define-method (bits (self metaclass)) nbits)
      (define-method (signed? (self metaclass)) (eq? sgn 'signed)))))

(define-method (bits (value <int<>>)) (bits (class-of value)))
(define-method (signed? (value <int<>>)) (signed? (class-of value)))

(define <ubyte> (integer  8 unsigned)) (define <meta<ubyte>> (class-of <ubyte>))
(define <byte>  (integer  8 signed  )) (define <meta<byte>>  (class-of <byte> ))
(define <usint> (integer 16 unsigned)) (define <meta<usint>> (class-of <usint>))
(define <sint>  (integer 16 signed  )) (define <meta<sint>>  (class-of <sint> ))
(define <uint>  (integer 32 unsigned)) (define <meta<uint>>  (class-of <uint> ))
(define <int>   (integer 32 signed  )) (define <meta<int>>   (class-of <int>  ))
(define <ulong> (integer 64 unsigned)) (define <meta<ulong>> (class-of <ulong>))
(define <long>  (integer 64 signed  )) (define <meta<long>>  (class-of <long> ))

(define single-precision 'single)
(define double-precision 'double)

(define (floating-point precision)
  (template-class (float precision) <float<>>
    (lambda (class metaclass)
      (define-method (double-precision? (self metaclass)) (eq? precision double-precision)) )))

(define <float>  (floating-point single-precision)) (define <meta<float>>  (class-of <float> ))
(define <double> (floating-point double-precision)) (define <meta<double>> (class-of <double>))

(define-structure complex make-rectangular (real-part imag-part))

(define <complex<float>>  (complex <float> )) (define <meta<complex<float>>>  (class-of (complex <float> )))
(define <complex<double>> (complex <double>)) (define <meta<complex<double>>> (class-of (complex <double>)))

(define-generic target)

(define (pointer tgt)
  "Create pointer class"
  (template-class (pointer tgt) <pointer<>>
    (lambda (class metaclass)
      (define-method (target (self metaclass)) tgt))))

(define-method (typecode (self <llvmlist<>>)) (typecode (class-of self)))

(define-method (dimension (self <llvmlist<>>)) (dimension (class-of self)))

(define-method (llvmlist (type <meta<void>>) (size <integer>))
  (template-class (llvmlist type size) <llvmlist<>>
    (lambda (class metaclass)
      (define-method (typecode (self metaclass)) type)
      (define-method (dimension (self metaclass)) size)
      (define-method (base (self metaclass)) (make-list size type))
      (define-method (components (self metaclass))
        (map (lambda (index) (cut get <> index)) (iota size))))))

(define-method (constructor (type <meta<llvmlist<>>>))
  "Get constructor for static size list"
  list)

(define-method (get (self <list>) index)
  "Get element of llvmlist"
  (list-ref self index))

(define-method (get (self <llvmlist<>>) index)
  "Element access for static size list in compiled code"
  (make (typecode self) #:value (lambda (fun) (list-ref ((get self) fun) index))))

(define-class* <multiarray<>> <object> <meta<multiarray<>>> <class>
               (shape       #:init-keyword #:shape       #:getter shape      )
               (strides     #:init-keyword #:strides     #:getter strides    )
               (memory      #:init-keyword #:memory      #:getter memory     )
               (memory-base #:init-keyword #:memory-base #:getter memory-base))

(define-method (initialize (self <multiarray<>>) initargs)
  (let-keywords initargs #f (memory memory-base shape strides allocator)
    (let* [(allocator   (or allocator gc-malloc-pointerless))
           (memory      (or memory (allocator (apply * (size-of (typecode self)) shape))))
           (memory-base (or memory-base memory))
           (strides     (or strides (map (compose (cut apply * <>) (cut list-head shape <>)) (iota (length shape)))))]
      (next-method self (list #:memory memory #:shape shape #:strides strides #:memory-base memory-base)))))

(define-method (typecode (self <multiarray<>>)) (typecode (class-of self)))

(define-method (dimensions (self <multiarray<>>)) (dimensions (class-of self)))

(define (multiarray type dim)
  "Define multi-dimensional array"
  (template-class (multiarray type dim) <multiarray<>>
    (lambda (class metaclass)
      (define-method (dimensions (self metaclass)) dim)
      (define-method (typecode  (self metaclass)) type))))

(define-class* <llvmarray<>> <structure> <meta<llvmarray<>>> <meta<structure>>)

(define-method (typecode (self <llvmarray<>>)) (typecode (class-of self)))

(define-method (dimension (self <llvmarray<>>)) (dimension (class-of self)))

(define (llvmarray type dim)
  "Define compiled multi-dimensional array"
  (template-class (llvmarray type dim) <llvmarray<>>
    (lambda (class metaclass)
      (define-method (dimensions (self metaclass)) dim)
      (define-method (typecode (self metaclass)) type)
      (define-method (base (self metaclass))
        (list (pointer type) (pointer type) (llvmlist <int> dim) (llvmlist <int> dim)))
      (define-method (constructor (self metaclass))
        (lambda (memory memory-base shape strides)
          (make (multiarray type dim)
                #:memory memory
                #:memory-base memory-base
                #:shape shape
                #:strides strides))))))

(component-accessors <llvmarray<>> <meta<llvmarray<>>> memory memory-base shape strides)

(define-method (equal? (a <void>) (b <void>))
  (equal? (get a) (get b)))

(define-method (base (type <meta<void>>))
  '())

(define-method (base (type <meta<scalar>>))
  (list type))

(define-method (foreign-type (type <meta<void>>))
  void)

(define-method (foreign-type (type <meta<bool>>))
  bool)

(define-method (foreign-type (type <meta<int<>>>))
  "Get foreign type for integer type"
  (- (* 2 (inexact->exact (/ (log (bits type)) (log 2)))) (if (signed? type) 2 3)))

(define-method (foreign-type (type <meta<float<>>>))
  "Get foreign type for floating-point type"
  (if (double-precision? type) double float))

(define-method (foreign-type (type <meta<pointer<>>>))
  "Get foreing type of pointer"
  int64)

(define-method (foreign-type (type <meta<llvmlist<>>>))
  "Get foreign type of static size list"
  int64)

(define-method (foreign-type (type <meta<llvmarray<>>>))
  "Get foreign type of multi-dimensional array"
  int64)

(define-method (size-of (type <meta<bool>>))
  1)

(define-method (size-of (type <meta<int<>>>))
  "Get size of integer values"
  (/ (bits type) 8))

(define-method (size-of (type <meta<float<>>>))
  "Get size of floating-point values"
  (if (double-precision? type) 8 4))

(define-method (size-of (type <meta<void>>))
  "Determine size of type"
  (apply + (map size-of (base type))))

(define-method (size-of (type <meta<pointer<>>>))
  "Size of pointer"
  8)

(define-method (native-type (value <boolean>))
  <bool>)

(define-method (native-type (value <integer>))
  (if (>= value 0)
    (cond ((< value (ash 1  8)) <ubyte>)
          ((< value (ash 1 16)) <usint>)
          ((< value (ash 1 32)) <uint> )
          (else <ulong>))
    (let [(nvalue (lognot value))]
      (cond ((< nvalue (ash 1  7)) <byte>)
            ((< nvalue (ash 1 15)) <sint>)
            ((< nvalue (ash 1 31))  <int>)
            (else <long>)))))

(define-method (native-type (value <real>))
  <double>)

(define-method (native-type (value <complex>))
  <complex<double>>)

(define-method (native-type (value <list>))
  (llvmlist (reduce coerce #f (map native-type value)) (length value)))

(define-method (native-type (value <multiarray<>>))
  (llvmarray (typecode value) (dimensions value)))

(define-method (unpack-value (self <meta<void>>) address)
  address)

(define-method (unpack-value (self <meta<bool>>) address)
  (not (equal? (pointer->bytevector (make-pointer address) (size-of self)) #vu8(0))))

(define-method (unpack-value (self <meta<int<>>>) (address <integer>))
  "Unpack integer stored in a byte vector"
  (let [(packed    (pointer->bytevector (make-pointer address) (size-of self)))
        (converter (if (signed? self) bytevector->sint-list bytevector->uint-list))]
    (car (converter packed (native-endianness) (bytevector-length packed)))))

(define-method (unpack-value (self <meta<float<>>>) (address <integer>))
  "Unpack floating-point value stored in a byte vector"
    (let [(packed    (pointer->bytevector (make-pointer address) (size-of self)))
          (converter (if (double-precision? self) bytevector-ieee-double-native-ref bytevector-ieee-single-native-ref))]
      (converter packed 0)))

(define-method (unpack-value (self <meta<pointer<>>>) address)
  (make-pointer (car (bytevector->sint-list (pointer->bytevector (make-pointer address) 8) (native-endianness) 8))))

(define-method (unpack-value (self <meta<structure>>) (address <integer>))
  "Unpack composite value stored in a byte vector"
  (apply (constructor self) (map (lambda (type offset) (unpack-value type (+ address offset)))
                                 (base self)
                                 (integral (cons 0 (all-but-last (map size-of (base self))))))))

(define-method (coerce (a <meta<int<>>>) (b <meta<int<>>>))
  "Type coercion for integers"
  (let* [(is-signed? (or (signed? a) (signed? b)))
         (to-signed  (lambda (t) (if (signed? t) t (integer (* 2 (bits t)) signed))))
         (adapt      (if (eq? (signed? a) (signed? b)) identity to-signed))]
    (integer (min 64 (max (bits (adapt a)) (bits (adapt b)))) (if is-signed? signed unsigned))))

(define-method (coerce (a <meta<float<>>>) (b <meta<float<>>>))
  "Coerce floating-point numbers"
  (if (double-precision? a) a b))

(define-method (coerce (a <meta<float<>>>) (b <meta<int<>>>))
  "Coerce floating-point number and integer"
  a)

(define-method (coerce (a <meta<int<>>>) (b <meta<float<>>>))
  "Coerce integer and floating-point number"
  b)

(define-method (coerce (a <meta<complex<>>>) (b <meta<complex<>>>))
  (complex (coerce (apply coerce (base a)) (apply coerce (base b)))))

(define-method (coerce (a <meta<scalar>>) (b <meta<complex<>>>))
  (complex (coerce a (apply coerce (base b)))))

(define-method (coerce (a <meta<complex<>>>) (b <meta<scalar>>))
  (complex (coerce (apply coerce (base a)) b)))

(define-method (coerce (a <meta<pointer<>>>) (b <meta<int<>>>))
  "Coerce pointers and integers"
  a)

(define-method (decompose-type (type <meta<scalar>>))
  "Decompose scalar type"
  (base type))

(define-method (decompose-type (type <meta<structure>>))
  "Decompose composite type"
  (append-map decompose-type (base type)))

(define-method (decompose-type (type <meta<pointer<>>>))
  (list <long>))

(define-method (decompose-argument (type <meta<scalar>>) value)
  "Decompose scalar value"
  (list value))

(define-method (decompose-argument (type <meta<bool>>) value)
  "Decompose boolean value"
  (list (if value 1 0)))

(define-method (decompose-argument (type <meta<structure>>) value)
  "Recursively decompose composite value"
  (append-map decompose-argument (base type) (map (cut <> value) (components type))))

(define-method (decompose-argument (type <meta<pointer<>>>) value)
  (list (pointer-address value)))

(define-method (decompose-result (type <meta<scalar>>) value)
  (list value))

(define-method (decompose-result (type <meta<structure>>) value)
  "Recursively decompose composite value"
  (append-map decompose-result (base type) (map (cut <> value) (components type))))

(define (compose-base base-types lst)
  (if (null? base-types)
    (cons (const '()) lst)
    (let* [(result (compose-content (car base-types) lst))
           (rest   (compose-base    (cdr base-types) (cdr result)))]
      (cons (lambda (fun) (cons ((car result) fun) ((car rest) fun))) (cdr rest)))))

(define-method (compose-content (type <meta<structure>>) lst)
  (compose-base (base type) lst))

(define-method (compose-content (type <meta<scalar>>) lst)
  (cons (car lst) (cdr lst)))

(define (compose-value type lst)
  "Compose a scalar value"
  (let [(content (compose-content type lst))]
    (cons (make type #:value (car content)) (cdr content))))

(define (compose-values types lst)
  "Compose multiple values"
  (if (null? types)
    '()
    (let [(result (compose-content (car types) lst))]
      (cons (make (car types) #:value (car result)) (compose-values (cdr types) (cdr result))))))
