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

(test-begin "identify free variables")
  (let [(a (var <int>))
        (b (var <int>))]
    (test-assert "no variables means no unallocated variables"
      (null? (unallocated-variables '())))
    (test-equal "return the unallocated variable"
      (list a) (unallocated-variables (list (cons a #f))))
    (test-assert "ignore the variable with register allocated"
      (null? (unallocated-variables (list (cons a RAX))))))
(test-end "identify free variables")

(test-begin "check for variables with register allocated")
  (let [(a (var <int>))]
    (test-assert "no variables means no variables with register allocated"
      (null? (register-allocations '())))
    (test-equal "return the variable with register allocation information"
      (list (cons a RAX)) (register-allocations (list (cons a RAX))))
    (test-assert "filter out the variable which does not have a register allocated"
      (null? (register-allocations (list (cons a #f))))))
(test-end "check for variables with register allocated")

(test-begin "count spilled variables")
  (test-eqv "count zero spilled variables"
    0 (number-spilled-variables '() '()))
  (test-eqv "count one spilled variable"
    1 (number-spilled-variables '((a . #f)) '()))
  (test-eqv "ignore allocated variables when counting spilled variables"
    0 (number-spilled-variables (list (cons (var <int>) RAX)) '()))
  (test-eqv "do not count stack parameters when allocating stack space"
    0 (number-spilled-variables '((a . #f)) '(a)))
  (test-eqv "allocate stack space if spilled variable is not a stack parameter"
    1 (number-spilled-variables '((a . #f)) '(b)))
(test-end "count spilled variables")

(test-begin "handling spill locations")
  (let [(a (var <int>))
        (b (var <int>))]
    (test-assert "assigning spill locations to an empty list of variables returns an empty list"
      (null?  (assign-spill-locations '() 16 8)))
    (test-equal "assign spill location to a variable"
      (list (cons a (ptr <long> RSP 16)))  (assign-spill-locations (list a) 16 8))
    (test-equal "assign spill location with a different offset"
      (list (cons a (ptr <long> RSP 32)))  (assign-spill-locations (list a) 32 8))
    (test-equal "use increasing offsets for spill locations"
      (list (cons a (ptr <long> RSP 16)) (cons b (ptr <long> RSP 24))) (assign-spill-locations (list a b) 16 8))
    (test-assert "do nothing if there are no variables"
      (null? (add-spill-information '() 16 8)))
    (test-equal "pass through variables with register allocation information"
      (list (cons a RAX)) (add-spill-information (list (cons a RAX)) 16 8))
    (test-equal "allocate spill location for a variable"
      (list (cons a (ptr <long> RSP 16))) (add-spill-information (list (cons a #f)) 16 8)))
(test-end "handling spill locations")

(test-begin "blocked variables")
  (test-assert "no predefined variables"
    (null? (blocked-predefined '() '() '())))
  (test-equal "detect predefined variable with blocked register"
    (list (cons 'a RDI)) (blocked-predefined (list (cons 'a RDI)) '((a . (0 . 2))) (list (cons RDI '(1 . 3)))))
  (test-assert "ignore predefined variables if no registers are blocked"
    (null? (blocked-predefined (list (cons 'a RDI)) '((a . (0 . 2))) '())))
  (test-assert "ignore predefined variables if the associated register is not blocked"
    (null? (blocked-predefined (list (cons 'a RDI)) '((a . (0 . 2))) (list (cons RDI '(3 . 4))))))
  (test-assert "ignore unused variable when checking for blocked registers"
    (null? (blocked-predefined (list (cons 'a RDI)) '() (list (cons RDI '(2 . 3))))))
  (test-assert "only consider register associated with variable when blocking"
    (null? (blocked-predefined (list (cons 'a RDI)) '((a . (0 . 2))) (list (cons RAX '(1 . 3))))))
  (test-assert "no predefined variables with blocked registers to move"
    (null? (move-blocked-predefined '())))
  (let [(a (var <int>))]
    (test-equal "copy variable from blocked register"
      (list (MOV a RAX)) (move-blocked-predefined (list (cons a RAX)))))
  (test-equal "no predefinitions to discard"
    (list (cons 'a RDI)) (non-blocked-predefined (list (cons 'a RDI)) '()))
  (test-equal "discard predefined variables which are blocked"
    '() (non-blocked-predefined (list (cons 'a RDI)) (list (cons 'a RDI))))
  (test-equal "only discard predefined variables which are blocked"
    (list (cons 'b RSI)) (non-blocked-predefined (list (cons 'a RDI) (cons 'b RSI)) (list (cons 'a RDI))))
(test-end "blocked variables")

(test-begin "linear scan allocation")
  (test-assert "linear scan with no variables returns empty mapping"
    (null? (linear-scan-coloring '() '() '() '())))
  (test-equal "allocate single variable"
    (list (cons 'a RAX)) (linear-scan-coloring '((a . (0 . 0))) (list RAX RCX) '() '()))
  (test-equal "reuse register with two variables"
    (list (cons 'a RAX) (cons 'b RAX)) (linear-scan-coloring '((a . (0 . 0)) (b . (1 . 1))) (list RAX RCX) '() '()))
  (test-equal "allocate different registers for two variables conflicting at index 1"
    (list (cons 'a RAX) (cons 'b RCX)) (linear-scan-coloring '((a . (0 . 1)) (b . (1 . 1))) (list RAX RCX) '() '()))
  (test-equal "sort live intervals by beginning of interval before performing linear-scan register allocation"
    (list (cons 'a RAX) (cons 'b RCX)) (linear-scan-coloring '((b . (1 . 1)) (a . (0 . 1))) (list RAX RCX) '() '()))
  (test-equal "allocate different registers for two variables conflicting at index 0"
    (list (cons 'a RAX) (cons 'b RCX)) (linear-scan-coloring '((a . (0 . 0)) (b . (0 . 1))) (list RAX RCX) '() '()))
  (test-equal "mark last variable for spilling if it has a longer live interval"
    (list (cons 'a RAX) (cons 'b #f)) (linear-scan-coloring '((a . (0 . 1)) (b . (1 . 3))) (list RAX) '() '()))
  (test-equal "mark first variable for spilling if it has a longer live interval"
    (list (cons 'a #f) (cons 'b RAX)) (linear-scan-coloring '((a . (0 . 3)) (b . (1 . 1))) (list RAX) '() '()))
  (test-equal "do not spill same variable twice"
    (list (cons 'a #f) (cons 'b #f) (cons 'c RAX))
    (linear-scan-coloring '((a . (0 . 5)) (b . (1 . 4)) (c . (2 . 3))) (list RAX) '() '()))
  (test-equal "use predefined register for variable"
    (list (cons 'a RCX)) (linear-scan-coloring '((a . (0 . 0))) (list RAX RCX) (list (cons 'a RCX)) '()))
  (test-equal "predefined registers take priority over normal register allocations"
    (list (cons 'a RCX) (cons 'b RAX))
    (linear-scan-coloring '((a . (0 . 1)) (b . (1 . 1))) (list RAX RCX) (list (cons 'a RCX)) '()))
  (let [(a (var <int>))]
    (test-equal "do not allocate register if it is blocked while the variable is live"
      (list (cons a RCX))
      (linear-scan-coloring (list (cons a '(0 . 1))) (list RAX RCX) '() (list (cons RAX '(1 . 2))))))
(test-end "linear scan allocation")
(test-end "aiscm register-allocate")
