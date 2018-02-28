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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (aiscm util)
  #:export (get integer signed unsigned bits signed? coerce foreign-type
            floating-point single-precision double-precision double-precision?
            decompose-argument decompose-arguments decompose-type decompose-types compose-value compose-values
            complex base size-of unpack-value native-type components
            <void> <meta<void>>
            <scalar> <meta<scalar>>
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
            <complex<double>> <meta<complex<double>>> <complex<float<double>>> <meta<complex<float<double>>>>)
  #:export-syntax (define-structure)
  #:re-export (real-part imag-part))


(define signed   'signed)
(define unsigned 'unsigned)

(define-class* <void> <object> <meta<void>> <class>
               (value #:init-keyword #:value #:getter get))

(define-method (foreign-type (type <meta<void>>))
  void)

(define-method (components (type <meta<void>>))
  '())

(define-method (unpack-value (self <meta<void>>) address)
  address)

(define-class* <scalar> <void> <meta<scalar>> <meta<void>>)

(define-method (equal? (a <scalar>) (b <scalar>))
  (equal? (get a) (get b)))

(define-class* <int<>> <scalar> <meta<int<>>> <meta<scalar>>)

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

(define-method (unpack-value (self <meta<int<>>>) (address <integer>))
  "Unpack integer stored in a byte vector"
  (let [(packed    (pointer->bytevector (make-pointer address) (size-of self)))
        (converter (if (signed? self) bytevector->sint-list bytevector->uint-list))]
    (car (converter packed (native-endianness) (bytevector-length packed)))))

(define-method (coerce (a <meta<int<>>>) (b <meta<int<>>>))
  "Type coercion for integers"
  (let* [(is-signed? (or (signed? a) (signed? b)))
         (to-signed  (lambda (t) (if (signed? t) t (integer (* 2 (bits t)) signed))))
         (adapt      (if (eq? (signed? a) (signed? b)) identity to-signed))]
    (integer (min 64 (max (bits (adapt a)) (bits (adapt b)))) (if is-signed? signed unsigned))))

(define-method (foreign-type (type <meta<int<>>>))
  "Get foreign type for integer type"
  (- (* 2 (inexact->exact (/ (log (bits type)) (log 2)))) (if (signed? type) 2 3)))

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

(define-method (size-of (type <meta<int<>>>))
  "Get size of integer values"
  (/ (bits type) 8))

(define single-precision 'single)
(define double-precision 'double)

(define-class* <float<>> <scalar> <meta<float<>>> <meta<scalar>>)

(define (floating-point precision)
  (template-class (float precision) <float<>>
    (lambda (class metaclass)
      (define-method (double-precision? (self metaclass)) (eq? precision double-precision)) )))

(define <float>  (floating-point single-precision)) (define <meta<float>>  (class-of <float> ))
(define <double> (floating-point double-precision)) (define <meta<double>> (class-of <double>))

(define-method (unpack-value (self <meta<float<>>>) (address <integer>))
  "Unpack floating-point value stored in a byte vector"
    (let [(packed    (pointer->bytevector (make-pointer address) (size-of self)))
          (converter (if (double-precision? self) bytevector-ieee-double-native-ref bytevector-ieee-single-native-ref))]
      (converter packed 0)))

(define-method (foreign-type (type <meta<float<>>>))
  "Get foreign type for floating-point type"
  (if (double-precision? type) double float))

(define-method (size-of (type <meta<float<>>>))
  "Get size of floating-point values"
  (if (double-precision? type) 8 4))

(define-method (coerce (a <meta<float<>>>) (b <meta<float<>>>))
  "Coerce floating-point numbers"
  (if (double-precision? a) a b))

(define-method (coerce (a <meta<float<>>>) (b <meta<int<>>>))
  "Coerce floating-point number and integer"
  a)

(define-method (coerce (a <meta<int<>>>) (b <meta<float<>>>))
  "Coerce integer and floating-point number"
  b)

(define-method (decompose-argument (type <meta<scalar>>) value)
  "Decompose scalar value"
  (list value))

(define-method (decompose-type (type <meta<scalar>>))
  "Decompose scalar type"
  (list type))

(define (decompose-types lst)
  "Decompose list of types"
  (concatenate (map decompose-type lst)))

(define (compose-value type lst)
  "Compose a value"
  (make type #:value (lambda (fun) (append-map (cut <> fun) lst))))

(define (compose-values types lst)
  "Compose multiple values"
  (if (null? types)
    '()
    (let* [(type       (car types))
           (base-types (decompose-type type))
           (count      (length base-types))]
      (cons (compose-value type (take lst count)) (compose-values (cdr types) (drop lst count))))))

(define-syntax define-structure
  (lambda (x)
    (syntax-case x ()
      ((k name constructor (members ...))
        (let [(class       (string->symbol (format #f "<~a<>>" (syntax->datum #'name))))
              (metaclass   (string->symbol (format #f "<meta<~a<>>>" (syntax->datum #'name))))
              (n           (length (syntax->datum #'(members ...))))
              (cdr-members (cdr (syntax->datum #'(members ...))))]
          #`(begin
              (define-class* #,(datum->syntax #'k class) <void> #,(datum->syntax #'k metaclass) <meta<void>>)
              (define-method (construct-from-composite (type #,(datum->syntax #'k metaclass)) arguments)
                "Construct Scheme object from composite type"
                (apply constructor arguments))
              (define-method (name (initial <meta<void>>) #,@(datum->syntax #'k cdr-members))
                "Instantiate a composite type using the type template"
                (template-class (name initial #,@(datum->syntax #'k cdr-members)) #,(datum->syntax #'k class)
                  (lambda (class metaclass) #f)))
              (define-method (name (base-type <meta<void>>))
                "Instantiate a composite type using the type template"
                (template-class (name base-type) #,(datum->syntax #'k class)
                  (lambda (class metaclass)
                    (define-method (base (self metaclass)) base-type))))
              (define-method (foreign-type (type #,(datum->syntax #'k metaclass)))
                "Foreign type of template class is pointer"
                int64)
              (define-method (components (type #,(datum->syntax #'k metaclass)))
                "List component accessor methods of composite type"
                (list members ...))
              (begin .
                #,(map (lambda (member-name index)
                         #`(define-method (#,(datum->syntax #'k member-name) self)
                             "Access a component of the composite type"
                             (make (base (class-of self))
                                   #:value (lambda (fun) (list (list-ref ((get self) fun) #,(datum->syntax #'k index)))))))
                       (syntax->datum #'(members ...))
                       (iota n)))))))))

(define-method (size-of (type <meta<void>>))
  (if (null? (components type)) 0 (* (length (components type)) (size-of (base type)))))

(define-method (decompose-argument (type <meta<void>>) value)
  "Decompose composite value"
  (map (cut <> value) (components type)))

(define (decompose-arguments types lst)
  "Decompose multiple values"
  (concatenate (map decompose-argument types lst)))

(define-method (decompose-type (type <meta<void>>))
  "Decompose composite type"
  (make-list (length (components type)) (base type)))

(define-method (unpack-value (self <meta<void>>) (address <integer>))
  "Unpack composite value stored in a byte vector"
  (construct-from-composite self
                            (map (lambda (offset) (unpack-value (base self) (+ address offset)))
                                 (iota (length (components self)) 0 (size-of (base self))))))

(define-structure complex make-rectangular (real-part imag-part))

(define <complex<float>>  (complex <float> )) (define <meta<complex<float>>>  (class-of (complex <float> )))
(define <complex<double>> (complex <double>)) (define <meta<complex<double>>> (class-of (complex <double>)))
