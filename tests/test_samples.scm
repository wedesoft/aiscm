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
             (aiscm mem)
             (aiscm sequence)
             (aiscm samples))


(test-begin "aiscm samples")

(define l '((2 3) (5 7) (11 13) (17 19)))
(define m (to-array <sint> l))
(define mem (value m))
(define samples (make <samples> #:typecode <sint> #:shape '(2 4) #:rate 44100 #:planar #f #:mem mem))
(define array (arr <int> (2 3) (5 7) (11 13) (17 19)))

(test-eq "query typecode of samples"
  <sint> (typecode samples))
(test-equal "query shape of samples"
  '(2 4) (shape samples))
(test-eqv "query number of channels"
  2 (channels samples))
(test-eqv "query sampling rate"
  44100 (rate samples))
(test-assert "query whether samples are planar"
  (not (planar? samples)))
(test-eq "check data is memorized"
  mem (slot-ref samples 'mem))
(test-equal "'to-array' should convert the audio samples to a 2D array"
  l (to-list (to-array samples)))
(test-eq "check sample type when converting array to samples"
  <int> (typecode (to-samples array)))
(test-assert "check samples are not marked as planar"
  (not (planar? (to-samples array))))
(test-equal "check samples converted from array have the right shape"
  '(2 4) (shape (to-samples array)))
(test-eq "check sample memory is initialised when converting from array"
  (slot-ref array 'value) (slot-ref (to-samples array) 'mem))
(test-equal "packed audio has one offset which is zero"
  '(0) (slot-ref samples 'offsets))
(test-eq "convert samples to integer"
  <int> (typecode (convert-samples samples <int> 44100 #f)))
(test-eqv "size of converted sample data"
  32 (slot-ref (slot-ref (convert-samples samples <int> 44100 #f) 'mem) 'size))
(test-expect-fail 1)
(test-equal "content of converted array"
  l (to-list (to-array (convert-samples samples <int> 44100 #f))))

(test-end "aiscm samples")
