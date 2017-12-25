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
  #:use-module (aiscm util)
  #:export (get integer signed unsigned bits signed? coerce foreign-type
            <int<>> <meta<int<>>>
            <ubyte> <meta<ubyte>> <int<8,unsigned>>  <meta<int<8,unsigned>>>
            <byte>  <meta<byte>>  <int<8,signed>>    <meta<int<8,signed>>>
            <usint> <meta<usint>> <int<16,unsigned>> <meta<int<16,unsigned>>>
            <sint>  <meta<sint>>  <int<16,signed>>   <meta<int<16,signed>>>
            <uint>  <meta<uint>>  <int<32,unsigned>> <meta<int<32,unsigned>>>
            <int>   <meta<int>>   <int<32,signed>>   <meta<int<32,signed>>>
            <ulong> <meta<ulong>> <int<64,unsigned>> <meta<int<64,unsigned>>>
            <long>  <meta<long>>  <int<64,signed>>   <meta<int<64,signed>>>))


(define signed   'signed)
(define unsigned 'unsigned)

(define-class* <int<>> <object> <meta<int<>>> <class>
               (value #:init-keyword #:value #:getter get))

(define-method (equal? (a <int<>>) (b <int<>>))
  (equal? (get a) (get b)))

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

(define (coerce a b)
  "Type coercion for integers"
  (let* [(is-signed? (or (signed? a) (signed? b)))
         (to-signed  (lambda (t) (if (signed? t) t (integer (* 2 (bits t)) signed))))
         (adapt      (if (eq? (signed? a) (signed? b)) identity to-signed))]
    (integer (min 64 (max (bits (adapt a)) (bits (adapt b)))) (if is-signed? signed unsigned))))

(define (foreign-type type)
  "Get foreign type for integer type"
  (- (* 2 (inexact->exact (/ (log (bits type)) (log 2)))) (if (signed? type) 2 3)))
