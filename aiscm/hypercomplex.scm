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
(define-module (aiscm hypercomplex)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (aiscm core)
  #:export (make-hypercomplex jmag-part kmag-part hypercomplex
            <hypercomplex>
            <hypercomplex<>>
            <hypercomplex<float>>  <meta<hypercomplex<float>>>  <hypercomplex<float<single>>> <meta<hypercomplex<float<single>>>>
            <hypercomplex<double>> <meta<hypercomplex<double>>> <hypercomplex<float<double>>> <meta<hypercomplex<float<double>>>>)
  #:re-export (real-part imag-part equal?))


(define-class <hypercomplex> ()
  (real #:init-keyword #:real-part #:getter real-part)
  (imag #:init-keyword #:imag-part #:getter imag-part)
  (jmag #:init-keyword #:jmag-part #:getter jmag-part)
  (kmag #:init-keyword #:kmag-part #:getter kmag-part))

(define (make-hypercomplex a b c d)
  (make <hypercomplex> #:real-part a #:imag-part b #:jmag-part c #:kmag-part d))

(define-method (write (self <hypercomplex>) port)
  (format port "~f~@fi~@fj~@fk" (real-part self) (imag-part self) (jmag-part self) (kmag-part self)))

(define-method (equal? (a <hypercomplex>) (b <hypercomplex>))
  (and (equal? (real-part a) (real-part b))
       (equal? (imag-part a) (imag-part b))
       (equal? (jmag-part a) (jmag-part b))
       (equal? (kmag-part a) (kmag-part b))))

(define-structure hypercomplex make-hypercomplex (real-part imag-part jmag-part kmag-part))
(define-uniform-constructor hypercomplex)

(define <hypercomplex<float>>  (hypercomplex <float> )) (define <meta<hypercomplex<float>>>  (class-of (hypercomplex <float> )))
(define <hypercomplex<double>> (hypercomplex <double>)) (define <meta<hypercomplex<double>>> (class-of (hypercomplex <double>)))
