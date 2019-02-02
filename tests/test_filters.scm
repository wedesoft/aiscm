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
             (aiscm core)
             (aiscm filters))

(test-begin "aiscm filters")

(test-group "gauss-filter"
  (test-equal "Trivial Gauss filter"
    '(1.0) (to-list (gauss-filter 1.0 #:size 1)))
  (test-equal "Filter with 3 elements"
    '(3) (shape (gauss-filter 1.0 #:size 3)))
  (test-approximate "Sum of elements is 1"
    1.0 (sum (gauss-filter 1.0 #:size 3)) 1e-5)
  (test-assert "Maximum is larger than neighbouring values"
    (< 0.4 (max (gauss-filter 1.0 #:size 3))))
  (test-equal "Default filter size for sigma 1.0"
    '(5) (shape (gauss-filter 1.0)))
  (test-equal "Default filter size for sigma 2.0"
    '(9) (shape (gauss-filter 2.0))))

(test-group "gauss-gradient-filter"
  (test-equal "Trivial Gauss gradient filter"
    '(0.5 0.0 -0.5) (to-list (gauss-gradient-filter 1.0 #:size 3)))
  (test-equal "Filter with 5 elements"
    '(5) (shape (gauss-gradient-filter 1.0 #:size 5)))
  (test-equal "Gauss gradient determines gradient of ramp"
    1.0 (sum (* (gauss-gradient-filter 1.0 #:size 5) (arr 4 3 2 1 0))))
  (test-equal "Default filter size for sigma 1.0"
    '(5) (shape (gauss-gradient-filter 1.0)))
  (test-equal "Default filter size for sigma 2.0"
    '(9) (shape (gauss-gradient-filter 2.0))))

(test-equal "Size of gauss-blur result"
  '(240 320) (shape (gauss-blur (fill <ubyte> '(240 320) 0) 1.0)))

(test-end "aiscm filters")
