;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 Jan Wedekind <jan@wedesoft.de>
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
(define-module (aiscm int)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (aiscm element)
  #:use-module (aiscm scalar)
  #:use-module (aiscm bool)
  #:use-module (aiscm util)
  #:export (signed unsigned bits signed? integer
            <int<>> <meta<int<>>>
            <ubyte> <meta<ubyte>> <int<8,unsigned>>  <meta<int<8,unsigned>>>
            <byte>  <meta<byte>>  <int<8,signed>>    <meta<int<8,signed>>>
            <usint> <meta<usint>> <int<16,unsigned>> <meta<int<16,unsigned>>>
            <sint>  <meta<sint>>  <int<16,signed>>   <meta<int<16,signed>>>
            <uint>  <meta<uint>>  <int<32,unsigned>> <meta<int<32,unsigned>>>
            <int>   <meta<int>>   <int<32,signed>>   <meta<int<32,signed>>>
            <ulong> <meta<ulong>> <int<64,unsigned>> <meta<int<64,unsigned>>>
            <long>  <meta<long>>  <int<64,signed>>   <meta<int<64,signed>>>
            <native-int>
            ~ & | ^ << >> % =0 !=0 conj))
(define signed 'signed)
(define unsigned 'unsigned)
(define-class* <int<>> <scalar> <meta<int<>>> <meta<scalar>>)
(define-method (write (self <int<>>) port)
  (format port "#<~a ~a>" (class-name (class-of self)) (get self)))
(define-method (signed? (self <element>)) (signed? (class-of self)))
(define-method (bits (self <int<>>)) (bits (class-of self)))
(define (integer nbits sgn)
  (template-class (int nbits sgn) <int<>>
    (lambda (class metaclass)
      (define-method (bits (self metaclass)) nbits)
      (define-method (signed? (self metaclass)) (eq? sgn 'signed))
      (define-method (size-of (self metaclass)) (quotient (+ nbits 7) 8)))))
(define native-bits (* (sizeof '*) 8))
(define <ubyte> (integer  8 unsigned)) (define <meta<ubyte>> (class-of <ubyte>))
(define <byte>  (integer  8 signed  )) (define <meta<byte>>  (class-of <byte> ))
(define <usint> (integer 16 unsigned)) (define <meta<usint>> (class-of <usint>))
(define <sint>  (integer 16 signed  )) (define <meta<sint>>  (class-of <sint> ))
(define <uint>  (integer 32 unsigned)) (define <meta<uint>>  (class-of <uint> ))
(define <int>   (integer 32 signed  )) (define <meta<int>>   (class-of <int>  ))
(define <ulong> (integer 64 unsigned)) (define <meta<ulong>> (class-of <ulong>))
(define <long>  (integer 64 signed  )) (define <meta<long>>  (class-of <long> ))
(define <native-int> (integer native-bits signed))
(define-method (foreign-type (t <meta<int<8,unsigned>>> ))  uint8)
(define-method (foreign-type (t <meta<int<8,signed>>>   ))   int8)
(define-method (foreign-type (t <meta<int<16,unsigned>>>)) uint16)
(define-method (foreign-type (t <meta<int<16,signed>>>  ))  int16)
(define-method (foreign-type (t <meta<int<32,unsigned>>>)) uint32)
(define-method (foreign-type (t <meta<int<32,signed>>>  ))  int32)
(define-method (foreign-type (t <meta<int<64,unsigned>>>)) uint64)
(define-method (foreign-type (t <meta<int<64,signed>>>  ))  int64)
(define-method (pack (self <int<>>))
  (let [(converter (if (signed? self) sint-list->bytevector uint-list->bytevector))]
    (converter (list (inexact->exact (get self))) (native-endianness) (size-of self))))
(define-method (unpack (self <meta<int<>>>) (packed <bytevector>))
  (let* [(converter (if (signed? self) bytevector->sint-list bytevector->uint-list))
         (value     (car (converter packed (native-endianness) (size-of self))))]
    (make self #:value value)))
(define-method (coerce (a <meta<int<>>>) (b <meta<int<>>>))
  (let [(max-bits  (min 64 (max (bits a) (bits b))))
        (to-signed (lambda (t) (integer (* 2 (bits t)) signed)))]
    (if (signed? a)
      (if (signed? b) (integer max-bits signed) (coerce a (to-signed b)))
      (if (signed? b) (coerce (to-signed a) b) (integer max-bits unsigned)))))
(define-method (native-type (i <integer>) . args)
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
    (apply native-type (sort-by-pred (cons i args) integer?))))
(define-method (~ (self <integer>)) (lognot self))
(define-method (=0 (self <integer>)) (zero? self))
(define-method (!=0 (self <integer>)) (not (zero? self)))
(define-method (& a) a)
(define-method (& (a <integer>) (b <integer>)) (logand a b))
(define-method (& a b c . args) (apply & (cons (& a b) (cons c args))))
(define-method (| a) a)
(define-method (| (a <integer>) (b <integer>)) (logior a b))
(define-method (| a b c . args) (apply | (cons (| a b) (cons c args))))
(define-method (^ a) a)
(define-method (^ (a <integer>) (b <integer>)) (logxor a b))
(define-method (^ a b c . args) (apply ^ (cons (^ a b) (cons c args))))

(define-method (<< (a <integer>) (b <integer>)) (ash a b))
(define-method (<< (a <integer>)) (<< a 1))

(define-method (>> (a <integer>) (b <integer>)) (ash a (- b)))
(define-method (>> (a <integer>)) (>> a 1))

(define-method (% (a <integer>) (b <integer>)) (modulo a b))
(define-method (== (a <integer>) (b <integer>)) (= a b))
(define-method (!= (a <integer>) (b <integer>)) (not (= a b)))
(define-method (conj (a <integer>)) a)
