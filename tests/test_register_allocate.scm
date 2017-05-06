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
             (aiscm register-allocate)
             (aiscm asm)
             (aiscm variable)
             (aiscm command)
             (aiscm int)
             (aiscm jit))

(test-begin "aiscm register-allocate")

(test-equal "initially availability points of registers are zero by default"
  (list (cons RAX 0) (cons RCX 0)) (initial-register-use (list RAX RCX)))

(test-begin "sort live intervals")
  (test-equal "pass through live intervals if they are already sorted"
    '((a . (0 . 3)) (b . (1 . 2))) (sort-live-intervals '((a . (0 . 3)) (b . (1 . 2))) '()))
  (test-equal "sort live intervals by start point"
    '((b . (1 . 3)) (a . (2 . 2))) (sort-live-intervals '((a . (2 . 2)) (b . (1 . 3))) '()))
  (test-equal "prioritise predefined variables when sorting live intervals"
    '((a . (1 . 3)) (b . (0 . 2))) (sort-live-intervals '((b . (0 . 2)) (a . (1 . 3))) '(a)))
  (test-equal "sort live intervals by length if start point is the same"
    '((tmp . (0 . 0)) (a . (0 . 1))) (sort-live-intervals '((a . (0 . 1)) (tmp . (0 . 0))) '()))
(test-end "sort live intervals")

(test-begin "register availability")
  (test-equal "first register available"
    RAX (find-available-register (list (cons RAX 0)) 0))
  (test-assert "first register not available"
    (not (find-available-register (list (cons RAX 1)) 0)))
  (test-equal "first register available at a later point in time"
    RAX (find-available-register (list (cons RAX 1)) 1))
  (test-equal "first register already available"
    RAX (find-available-register (list (cons RAX 0)) 1))
  (test-equal "second register is available"
    RCX (find-available-register (list (cons RAX 3) (cons RCX 2)) 2))
  (test-equal "mark first register as used"
    (list (cons RAX 4)) (mark-used-till (list (cons RAX 1)) RAX 3))
  (test-equal "keep track of unaffected registers"
    (list (cons RAX 4) (cons RCX 5)) (mark-used-till (list (cons RAX 1) (cons RCX 5)) RAX 3))
  (test-equal "mark second register as used"
    (list (cons RAX 1) (cons RCX 9)) (mark-used-till (list (cons RAX 1) (cons RCX 5)) RCX 8))
(test-end "register availability")

(test-begin "register spilling")
  (test-eq "spill the one variable if there is no other candidate"
    'a (spill-candidate '((a . 0))))
  (test-eq "spill second variable if it is allocated for a longer interval"
    'b (spill-candidate '((a . 0) (b . 1))))
  (test-eq "spill first variable if it is allocated for a longer interval"
    'a (spill-candidate '((a . 1) (b . 0))))
  (test-equal "do not ignore variables with allocated register"
    '((a . 2)) (ignore-spilled-variables '((a . 2)) (list (cons 'a RAX))))
  (test-assert "ignore spilled variables"
    (null? (ignore-spilled-variables '((a . 2)) (list (cons 'a #f)))))
  (test-equal "do not ignore variable if it does not have a location assigned"
    '((a . 2)) (ignore-spilled-variables '((a . 2)) '()))
  (test-equal "do not ignore register if it is not blocked"
    (list (cons RAX 2)) (ignore-blocked-registers (list (cons RAX 2)) '(3 . 5) '()))
  (test-assert "ignore register for allocation if it is blocked"
    (null? (ignore-blocked-registers (list (cons RAX 2)) '(3 . 5) (list (cons RAX '(5 . 6))))))
  (test-equal "do not ignore register if it is blocked outside the specified interval"
    (list (cons RAX 2)) (ignore-blocked-registers (list (cons RAX 2)) '(3 . 5) (list (cons RAX '(6 . 8)))))
(test-end "register spilling")

(test-begin "live analysis")
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
(test-end "live analysis")

(test-end "aiscm register-allocate")
