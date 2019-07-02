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
  #:export (make-hypercomplex jmag-part kmag-part
            <hypercomplex>)
  #:re-export (real-part imag-part))


(define-class <hypercomplex> ()
  (real #:init-keyword #:real-part #:getter real-part)
  (imag #:init-keyword #:imag-part #:getter imag-part)
  (jmag #:init-keyword #:jmag-part #:getter jmag-part)
  (kmag #:init-keyword #:kmag-part #:getter kmag-part))

(define (make-hypercomplex a b c d)
  (make <hypercomplex> #:real-part a #:imag-part b #:jmag-part c #:kmag-part d))

(define-method (write (self <hypercomplex>) port)
  (format port "~f~@fi~@fj~@fk" (real-part self) (imag-part self) (jmag-part self) (kmag-part self)))
