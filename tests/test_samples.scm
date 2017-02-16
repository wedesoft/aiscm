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
(use-modules (srfi srfi-64)
             (oop goops)
             (aiscm element)
             (aiscm int)
             (aiscm sequence)
             (aiscm samples))


(test-begin "aiscm samples")

(define l '((2 3) (5 7) (11 13) (17 19)))
(define m (to-array <sint> l))
(define mem (value m))
(define samples (make <samples> #:typecode <sint> #:shape '(2 4) #:planar #f #:mem mem))

(test-eq "query typecode of samples"
  <sint> (typecode samples))
(test-equal "query shape of samples"
  '(2 4) (shape samples))
(test-eqv "query number of channels"
  2 (channels samples))
(test-assert "query whether samples are planar"
  (not (planar? samples)))
(test-eq "check data is memorized"
  mem (slot-ref samples 'mem))

(test-end "aiscm samples")
