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
             (aiscm bool)
             (aiscm int)
             (aiscm float)
             (aiscm rgb)
             (aiscm obj)
             (aiscm pointer)
             (aiscm variable))


(test-begin "aiscm variable")
(test-begin "native equivalent")
  (test-assert "RGB does not have a native equivalent"
    (not (native-equivalent (rgb 1 2 3))))
  (test-eq "integer is it's own native equivalent"
    <int> (native-equivalent <int>))
  (test-eq "short integer is it's own native equivalent"
    <sint> (native-equivalent <sint>))
  (test-eq "byte is native equivalent of boolean"
    <ubyte> (native-equivalent <bool>))
  (test-eq "native equivalent of pointer is a 64 bit integer"
    <ulong> (native-equivalent (pointer <ubyte>)))
  (test-eq "native equivalent of Scheme reference is a 64 bit integer"
    <ulong> (native-equivalent <obj>))
  (test-eq "floating point number is it's own equivalent"
    <float> (native-equivalent <float>))
  (test-eq "single-precision floating point number is it's own equivalent"
    <double> (native-equivalent <double>))
(test-end "native equivalent")

(test-begin "\"var\"' shortcut")
  (test-eq "Shortcut for creating variables creates variables"
    <var> (class-of (var <int>)))
  (test-eq "Shortcut for  creating variables uses specified type"
    <byte> (typecode (var <byte>)))
  (test-eq "Boolean values are represented using unsigned byte"
    <ubyte> (typecode (var <bool>)))
(test-end "\"var\"' shortcut")
(test-end "aiscm variable")
