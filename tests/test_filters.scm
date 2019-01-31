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
    '(1.0) (to-list (gauss-filter 1.0 1)))
  (test-equal "Filter with 3 elements"
    '(3) (shape (gauss-filter 1.0 3)))
  (test-approximate "Sum of elements is 1"
    1.0 (sum (gauss-filter 1.0 3)) 1e-5)
  (test-assert "Maximum is larger than neighbouring values"
    (< 0.4 (max (gauss-filter 1.0 3)))))

(test-end "aiscm filters")
