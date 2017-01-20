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
(use-modules (oop goops)
             (aiscm util)
             (aiscm element)
             (aiscm bool)
             (aiscm int)
             (aiscm complex)
             (aiscm rgb)
             (aiscm pointer)
             (aiscm asm)
             (aiscm jit)
             (aiscm sequence)
             (aiscm op)
             (guile-tap))
(ok (equal? '(3 3 3) (to-list (fill <byte> '(3) 3)))
    "fill byte sequence")
(ok (equal? '(3 3 3) (to-list (fill <int> '(3) 3)))
    "fill integer sequence")

; ------------------------------------------------------------------------------

(run-tests)
