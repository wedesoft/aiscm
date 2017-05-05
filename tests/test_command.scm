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
             (aiscm variable)
             (aiscm command)
             (aiscm asm)
             (aiscm int))


(test-begin "aiscm command")

(test-begin "input and output variables")
  (let [(a (var <int>))
        (b (var <int>))
        (p (var <long>))]
    (test-equal "Get input variables of MOV"
      (list b) (input (MOV a b)))
    (test-equal "Get input variables of ADD"
      (list a b) (input (ADD a b)))
    (test-equal "Prevent duplication of input variables"
      (list a) (input (ADD a a)))
    (test-equal "Get output variables of MOV"
      (list a) (output (MOV a b)))
    (test-equal "Get output variables of ADD"
      (list a) (output (ADD a b)))
    (test-equal "Get input variables of command writing to address"
      (list b a) (input (MOV (ptr <int> a) b)))
    (test-equal "Get arguments of command"
      (list a 0) (get-args (MOV a 0)))
    (test-equal "Get variables of a program"
      (list a b) (variables (list (MOV a 0) (MOV b a))))
    (test-equal "Get variables of a program using a pointer"
      (list a p) (variables (list (MOV a 0) (MOV (ptr <int> p) a)))))
(test-end "input and output variables")

(test-end "aiscm command")
