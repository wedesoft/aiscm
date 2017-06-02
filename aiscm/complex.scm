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
(define-module (aiscm complex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm composite)
  #:use-module (aiscm int)
  #:use-module (aiscm pointer)
  #:use-module (aiscm sequence)
  #:use-module (aiscm asm)
  #:use-module (aiscm variable)
  #:use-module (aiscm command)
  #:use-module (aiscm expression)
  #:use-module (aiscm operation)
  #:use-module (aiscm jit)
  #:use-module (aiscm util)
  #:export (<internalcomplex>
            <complex<>> <meta<complex<>>>
            <pointer<complex<>>> <meta<pointer<complex<>>>>
            complex)
  #:re-export (<pointer<element>> <meta<pointer<element>>>
               real-part imag-part to-type conj - + * / += *= max= min= where))

(define ctx (make <context>))

(define-method (conj self) self)
(define-method (conj (self <complex>)) (make-rectangular (real-part self) (- (imag-part self))))
(define-class <internalcomplex> ()
  (real #:init-keyword #:real-part #:getter real-part)
  (imag #:init-keyword #:imag-part #:getter imag-part))
(define-method (complex re im) (make <internalcomplex> #:real-part re #:imag-part im))
(define-method (write (self <internalcomplex>) port)
  (format port "(complex ~a ~a)" (real-part self) (imag-part self)))
(define-class* <complex<>> <composite> <meta<complex<>>> <meta<composite>>)
(define-method (complex (t <meta<element>>))
  (template-class (complex t) <complex<>>
    (lambda (class metaclass)
      (define-method (base (self metaclass))t)
      (define-method (size-of (self metaclass)) (* 2 (size-of t))))))
(define-method (components (self <meta<complex<>>>)) (list real-part imag-part))
(define-method (complex (t <meta<sequence<>>>)) (multiarray (complex (typecode t)) (dimensions t)))
(define-method (complex (re <meta<element>>) (im <meta<element>>)) (complex (coerce re im)))

(define-method (real-part (self <int<>>)) self); TODO: use a number type
(define-method (imag-part (self <int<>>)) 0)

(define-method (real-part (self <complex<>>)) (make (base (class-of self)) #:value (real-part (get self))))
(define-method (imag-part (self <complex<>>)) (make (base (class-of self)) #:value (imag-part (get self))))
(define-method (pack (self <complex<>>)) (bytevector-concat (map pack (content (class-of self) self))))
(define-method (unpack (self <meta<complex<>>>) (packed <bytevector>))
  (let* [(size    (size-of (base self)))
         (vectors (map (cut bytevector-sub packed <> size) (map (cut * size <>) (iota 2))))]
    (make self #:value (apply make-rectangular (map (lambda (vec) (get (unpack (base self) vec))) vectors)))))
(define-method (coerce (a <meta<complex<>>>) (b <meta<element>>)) (complex (coerce (base a) b)))
(define-method (coerce (a <meta<element>>) (b <meta<complex<>>>)) (complex (coerce a (base b))))
(define-method (coerce (a <meta<complex<>>>) (b <meta<complex<>>>)) (complex (coerce (base a) (base b))))
(define-method (coerce (a <meta<complex<>>>) (b <meta<sequence<>>>)) (multiarray (coerce a (typecode b)) (dimensions b)))
(define-method (native-type (c <complex>) . args)
  (complex (apply native-type (concatenate (map-if (cut is-a? <> <complex>) (lambda (v) (map inexact->exact (deconstruct <complex<>> v))) list (cons c args))))))
(define-method (build (self <meta<complex<>>>) value) (apply make-rectangular value))
(define-method (unbuild (type <meta<complex<>>>) self)
  (append-map (cut unbuild (base type) <>) (map inexact->exact (deconstruct type self))))
(define-method (content (type <meta<complex<>>>) (self <complex<>>))
  (append-map (cut content (base type) <>) (deconstruct type self)))
(define-method (content (type <meta<complex<>>>) (self <internalcomplex>))
  (append-map (cut content (base type) <>) (deconstruct type self)))
(define-method (base (self <meta<sequence<>>>)) (multiarray (base (typecode self)) (dimensions self)))

(define-syntax-rule (unary-complex-op op)
  (define-method (op (a <internalcomplex>)) (apply complex (map op (content <complex<>> a)))))

(unary-complex-op -)

(define-method (+= (a <internalcomplex>) (b <internalcomplex>))
  (append-map += (content <complex<>> a) (content <complex<>> b)))
(define-method (*= (a <internalcomplex>) (b <internalcomplex>))
  (let [(intermediate (parameter (complex (type (real-part a)))))]
    (append (append-map code (content <complex<>> intermediate) (content <complex<>> (* a b)))
            (append-map code (content <complex<>> a) (content <complex<>> intermediate)))))
(define-method (max= (a <internalcomplex>) (b <internalcomplex>))
  (append-map max= (content <complex<>> a) (content <complex<>> b)))
(define-method (min= (a <internalcomplex>) (b <internalcomplex>))
  (append-map min= (content <complex<>> a) (content <complex<>> b)))

(define-syntax-rule (binary-complex-op op)
  (begin
    (define-method (op (a <internalcomplex>) b) (complex (op (real-part a) b) (imag-part a)))
    (define-method (op a (b <internalcomplex>)) (complex (op a (real-part b)) (imag-part b)))
    (define-method (op (a <internalcomplex>) (b <internalcomplex>))
      (apply complex (map op (content <complex<>> a) (content <complex<>> b))))))

(define-method (conj (self <int<>>)) self)
(define-method (conj (self <pointer<int<>>>)) self)
(define-method (conj (a <internalcomplex>)) (complex (real-part a) (- (imag-part a))))
(binary-complex-op +)
(binary-complex-op -)
(define-method (* (a <internalcomplex>) b) (apply complex (map (cut * <> b) (content <complex<>> a))))
(define-method (* a (b <internalcomplex>)) (apply complex (map (cut * a <>) (content <complex<>> b))))
(define-method (* (a <internalcomplex>) (b <internalcomplex>))
  (complex (- (* (real-part a) (real-part b)) (* (imag-part a) (imag-part b)))
           (+ (* (real-part a) (imag-part b)) (* (imag-part a) (real-part b)))))
(define-method (/ (a <internalcomplex>) b) (apply complex (map (cut / <> b) (content <complex<>> a))))
(define (arg2 b) (apply + (map * (content <complex<>> b) (content <complex<>> b))))
(define-method (/ a (b <internalcomplex>))
  (let [(denom (arg2 b))]
    (complex (/ (* a (real-part b)) denom) (- (/ (* a (imag-part b)) denom)))))
(define-method (/ (a <internalcomplex>) (b <internalcomplex>))
  (let [(denom (arg2 b))]
    (complex (/ (+ (* (real-part a) (real-part b)) (* (imag-part a) (imag-part b))) denom)
             (/ (- (* (imag-part a) (real-part b)) (* (real-part a) (imag-part b))) denom))))

(define-method (where (m <param>) (a <internalcomplex>) (b <internalcomplex>))
  (apply complex (map (cut where m <...>) (content <complex<>> a) (content <complex<>> b))))
(define-method (where (m <param>) (a <internalcomplex>)  b       )
  (apply complex (map (cut where m <...>) (content <complex<>> a) (list b (native-const (type b) 0)))))
(define-method (where (m <param>)  a        (b <internalcomplex>))
  (apply complex (map (cut where m <...>) (list a (native-const (type a) 0)) (content <complex<>> b))))

(define-method (var (self <meta<complex<>>>)) (let [(type (base self))] (complex (var type) (var type)))); TODO: test
(pointer <complex<>>)
(define-method (real-part (self <pointer<>>)) self)
(define-method (imag-part (self <pointer<>>)) 0)
(define-method (real-part (self <pointer<complex<>>>)) (component (typecode self) self 0))
(define-method (imag-part (self <pointer<complex<>>>)) (component (typecode self) self 1))

(define-operator-mapping real-part 1 <meta<element>> (unary-extract real-part))
(define-operator-mapping imag-part 1 <meta<element>> (unary-extract imag-part))
(define-operator-mapping conj      1 <meta<element>> (unary-extract conj     ))

(define-jit-method base real-part 1)
(define-jit-method base imag-part 1)
(define-jit-method identity conj 1)

(define-jit-method complex complex 2)

(define-method (decompose-value (target <meta<complex<>>>) x)
  (make <internalcomplex> #:real-part (parameter (real-part (delegate x)))
                          #:imag-part (parameter (imag-part (delegate x)))))

(define-method (to-type (target <meta<complex<>>>) (self <internalcomplex>))
  (apply complex (map (cut to-type (base target) <>) (content <complex<>> self))))
