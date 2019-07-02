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
(use-modules (srfi srfi-64)
             (srfi srfi-1)
             (oop goops)
             (system foreign)
             (rnrs bytevectors)
             (aiscm core)
             (aiscm hypercomplex))


(test-begin "aiscm hypercomplex")

(test-group "hypercomplex values"
  (test-eqv "real part"
    2 (real-part (make-hypercomplex 2 3 5 7)))
  (test-eqv "imag part"
    3 (imag-part (make-hypercomplex 2 3 5 7)))
  (test-eqv "jmag part"
    5 (jmag-part (make-hypercomplex 2 3 5 7)))
  (test-eqv "kmag part"
    7 (kmag-part (make-hypercomplex 2 3 5 7))))

(test-group "display or write string"
  (test-equal "write hypercomplex number"
    "1.0+2.0i+3.0j+4.0k" (call-with-output-string (cut write (make-hypercomplex 1 2 3 4) <>)))
  (test-equal "write hypercomplex number with negative components"
    "-1.0-2.0i-3.0j-4.0k" (call-with-output-string (cut display (make-hypercomplex -1 -2 -3 -4) <>))))

(test-group "hypercomplex native type"
  (test-eq "hypercomplex single precision"
    <hypercomplex<float>> (hypercomplex <float>))
  (test-eq "hypercomplex double precision"
    <hypercomplex<double>> (hypercomplex <double>)))

(test-end "aiscm hypercomplex")
