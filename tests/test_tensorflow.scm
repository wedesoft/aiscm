;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 Jan Wedekind <jan@wedesoft.de>
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
             (aiscm tensorflow)
             (aiscm core))

(test-begin "aiscm tensorflow")

(test-group "tensor type"
  (test-assert "conversion to tensor creates something"
    (to-tensor (arr 2 3 5)))
  (for-each (lambda (type)
    (test-equal (format #f "round trip of tensor type ~a" (class-name type))
      type (typecode (from-tensor (to-tensor (make (multiarray type 1) #:shape '(3)))))))
      (list <ubyte> <byte> <usint> <sint> <uint> <int> <ulong> <long>))
  (test-expect-fail 1)
  (test-equal "round trip of tensor shape"
    '(2 3 5) (shape (from-tensor (to-tensor (make (multiarray <int> 3) #:shape '(2 3 5)))))))

(test-end "aiscm tensorflow")
