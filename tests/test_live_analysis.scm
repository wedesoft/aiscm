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
             (aiscm live-analysis)
             (aiscm asm)
             (aiscm variable)
             (aiscm command)
             (aiscm int)
             (aiscm jit))

(test-begin "aiscm live-analysis")
  (test-equal "Get following indices for first statement in a program"
    '(1) (next-indices '() (MOV CX 0) 0))
  (test-equal "Get following indices for second statement in a program"
    '(2) (next-indices '() (MOV AX CX) 1))
  (test-assert "RET statement should not have any following indices"
    (null? (next-indices '() (RET) 2)))
  (test-equal "Get following indices for a jump statement"
    '(2) (next-indices '((a . 2)) (JMP 'a) 0))
  (test-equal "Get following indices for a conditional jump"
    '(1 2) (next-indices '((a . 2)) (JNE 'a) 0))

  (test-equal "Get following indices for a list of statements"
    '((1) (2) (3) (4 1) ())
    (flow (list (NOP) 'a (NOP) (JNE 'a) (RET))))

  (let [(a (var <int>))
        (b (var <int>))]
    (test-equal "Live-analysis for definition of unused variable"
      (list '() (list a) '()) (live-analysis (list 'x (MOV a 0) (RET)) '()))
    (test-equal "Live-analysis for definition and later use of a variable"
      (list (list a) (list a) (list b a) '()) (live-analysis (list (MOV a 0) (NOP) (MOV b a) (RET)) '()))
    (test-equal "Live-analysis with conditional jump statement"
      (list (list a) (list a) (list a) (list a) '()) (live-analysis (list (MOV a 0) 'x (ADD a 1) (JE 'x) (RET)) '()))
    (test-equal "results should be propagated backwards from the return statement"
      (list (list a) (list a)) (live-analysis (list (MOV a 0) (RET)) (list a))))
(test-end "aiscm live-analysis")
