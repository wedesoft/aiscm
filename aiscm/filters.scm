;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018, 2019 Jan Wedekind <jan@wedesoft.de>
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
(define-module (aiscm filters)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (system foreign)
  #:use-module (aiscm core)
  #:export (gauss-filter gauss-gradient-filter gauss-blur gauss-gradient-x gauss-gradient-y harris-stephens))

(define pi 3.141592653589793)

(define (sgn x) (if (positive? x) 1 -1))

(define erf (pointer->procedure double (dynamic-func "erf" (dynamic-link)) (list double)))

(define (normalize-const f) (/ f (sum f)))

(define (default-filter-size sigma)
  (1+ (* (inexact->exact (ceiling (* sigma 2))) 2)))

(define* (gauss-filter sigma #:key (size (default-filter-size sigma)))
  "Compute Gauss blur filter"
  (normalize-const
    (to-array
      (map (lambda (x)
             (let* [(a (- x (/ size 2)))
                    (b (+ a 1))
                    (scale (/ 1 (* (sqrt 2) sigma)))]
        (- (erf (* scale b)) (erf (* scale a)))))
        (iota size)))))

(define (gauss-blur img sigma)
  (let* [(f (gauss-filter sigma))
         (s (car (shape f)))]
    (convolve (convolve img (reshape f (list s 1))) (reshape f (list 1 s)))))

(define (gauss-like x sigma) (exp (- (/ (* x x) (* sigma sigma)))))

(define (normalize-linear f) (let [(ramp (indices (car (shape f))))] (/ f (- (sum (* f ramp))))))

(define* (gauss-gradient-filter sigma #:key (size (default-filter-size sigma)))
  "Compute Gauss gradient filter"
  (normalize-linear
    (to-array
      (map (lambda (x)
             (let* [(a (- x (/ size 2)))
                    (b (+ a 1))]
                (- (gauss-like b sigma) (gauss-like a sigma))))
        (iota size)))))

(define (gauss-gradient-x img sigma)
  (let* [(f (gauss-gradient-filter sigma))
         (g (gauss-filter sigma))
         (s (car (shape f)))]
    (convolve (convolve img (reshape g (list s 1))) (reshape f (list 1 s)))))

(define (gauss-gradient-y img sigma)
  (let* [(f (gauss-gradient-filter sigma))
         (g (gauss-filter sigma))
         (s (car (shape f)))]
    (convolve (convolve img (reshape f (list s 1))) (reshape g (list 1 s)))))

(define (harris-stephens img sigma k)
  (let* [(x           (gauss-gradient-x img sigma))
         (y           (gauss-gradient-y img sigma))
         (a           (gauss-blur (* x x) sigma))
         (b           (gauss-blur (* y y) sigma))
         (c           (gauss-blur (* x y) sigma))
         (trace       (+ a b))
         (determinant (- (* a b) (* c c)))]
    (- determinant (* trace trace k))))
