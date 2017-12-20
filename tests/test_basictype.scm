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
             (aiscm basictype))


(test-begin "aiscm basictype")
  (for-each
    (lambda (nbits sign cls)
      (test-eq (format #f "Class for ~a-bit ~a integer should be ~a" nbits sign (class-name cls))
        cls (integer nbits sign))
      (test-eqv (format #f "~a should have ~a bits" (class-name cls) nbits)
        nbits (bits cls))
      (test-eq (format #f "~a should be ~a" (class-name cls) sign)
        (eq? sign 'signed) (signed? cls)))
    (list 8 8 16 16 32 32 64 64)
    (list unsigned signed unsigned signed unsigned signed unsigned signed)
    (list <ubyte> <byte> <usint> <sint> <uint> <int> <ulong> <long>))
(test-end "aiscm basictype")
