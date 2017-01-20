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
             (rnrs bytevectors)
             (srfi srfi-1)
             (srfi srfi-26)
             (aiscm util)
             (aiscm asm)
             (aiscm mem)
             (aiscm jit)
             (aiscm element)
             (aiscm int)
             (aiscm pointer)
             (aiscm sequence)
             (aiscm bool)
             (aiscm rgb)
             (aiscm complex)
             (guile-tap))
(define ctx (make <context>))
(ok (equal? '(((2 3 4) (5 6 7))) (to-list (+ (arr ((1 2 3) (4 5 6))) 1)))
    "add scalar to 3D array")
(ok (equal? '(((2 3 4) (5 6 7))) (to-list (+ 1 (arr ((1 2 3) (4 5 6))))))
    "add 3D array to scalar")
(ok (equal? '(((2 4 6) (8 10 12))) (let [(m (arr ((1 2 3) (4 5 6))))] (to-list (+ m m))))
    "add two 3D arrays")
(ok (equal? '((((3 3) (3 3)) ((3 3) (3 3))) (((3 3) (3 3)) ((3 3) (3 3))))
            (to-list (+ (arr (((2 2) (2 2)) ((2 2) (2 2))) (((2 2) (2 2)) ((2 2) (2 2)))) 1)))
    "add 1 to 4D array")
(run-tests)

