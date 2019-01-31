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
  #:use-module (aiscm core)
  #:export (gauss-filter))

(define pi 3.141592653589793)

(define (sgn x) (if (positive? x) 1 -1))

; Approximation see https://en.wikipedia.org/wiki/Error_function
(define (erf x)
  (let* [(x2  (* x x))
         (ex2 (exp (- x2)))
         (spi (sqrt pi))]
    (* (/ 2 spi) (sgn x) (sqrt (- 1 ex2)) (+ (/ spi 2) (* (/ 31 200) ex2) (* (/ -341 8000) (exp (* -2 x2)))))))

(define (normalize f) (/ f (sum f)))

(define (gauss-filter sigma size)
  (let [(size2 (/ size 2))]
    (normalize
      (to-array
        (map (lambda (x)
               (let* [(a (- x size2))
                      (b (+ a 1))
                      (s (/ 1 (* (sqrt 2) sigma)))]
          (/ (- (erf (* s b)) (erf (* s a))) 2)))
          (iota size))))))
