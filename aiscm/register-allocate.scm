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
(define-module (aiscm register-allocate)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 curried-definitions)
  #:use-module (oop goops)
  #:use-module (aiscm util)
  #:use-module (aiscm int)
  #:use-module (aiscm asm)
  #:use-module (aiscm variable)
  #:use-module (aiscm command)
  #:use-module (aiscm program)
  #:export (initial-register-use sort-live-intervals find-available-register mark-used-till
            spill-candidate ignore-spilled-variables ignore-blocked-registers
            unallocated-variables register-allocations assign-spill-locations add-spill-information
            blocked-predefined move-blocked-predefined non-blocked-predefined linear-scan-coloring
            number-spilled-variables filter-blocks blocked-intervals))

(define (initial-register-use registers)
  "Initially all registers are available from index zero on"
  (map (cut cons <> 0) registers))

(define (sort-live-intervals live-intervals predefined-variables)
  "Sort live intervals predefined variables first and then lexically by start point and length of interval"
  (sort-by live-intervals
           (lambda (live) (if (memv (car live) predefined-variables) -1 (- (cadr live) (/ 1 (+ 2 (cddr live))))))))

(define (find-available-register availability first-index)
  "Find register available from the specified first program index onwards"
  (car (or (find (compose (cut <= <> first-index) cdr) availability) '(#f))))

(define (mark-used-till availability element last-index)
  "Mark element in use up to specified index"
  (assq-set availability element (1+ last-index)))

(define (spill-candidate variable-use)
  "Select variable blocking for the longest time as a spill candidate"
  (car (argmax cdr variable-use)))

(define (ignore-spilled-variables variable-use allocation)
  "Remove spilled variables from the variable use list"
  (filter (compose (lambda (var) (cdr (or (assq var allocation) (cons var #t)))) car) variable-use))

(define (unallocated-variables allocation)
   "Return a list of unallocated variables"
   (map car (filter (compose not cdr) allocation)))

(define (number-spilled-variables allocation stack-parameters)
  "Count the number of spilled variables"
  (length (difference (unallocated-variables allocation) stack-parameters)))

(define (register-allocations allocation)
   "Return a list of variables with register allocated"
   (filter cdr allocation))

(define (assign-spill-locations variables offset increment)
  "Assign spill locations to a list of variables"
  (map (lambda (variable index) (cons variable (ptr <long> RSP index)))
       variables
       (iota (length variables) offset increment)))

(define (add-spill-information allocation offset increment)
  "Allocate spill locations for spilled variables"
  (append (register-allocations allocation)
          (assign-spill-locations (unallocated-variables allocation) offset increment)))

(define (filter-blocks prog)
  (cond
    ((is-a? prog <block>) (filter-blocks (get-code prog)))
    ((list? prog)         (map filter-blocks prog))
    (else                 prog)))

(define (blocked-intervals prog)
  (define code-length (compose length flatten-code filter-blocks))
  (define ((bump-interval offset) interval)
    (cons (car interval) (cons (+ (cadr interval) offset) (+ (cddr interval) offset))))
  (cond
    ((is-a? prog <block>) (cons (cons (get-reg prog) (cons 0 (1- (code-length (get-code prog)))))
                            (blocked-intervals (get-code prog))))
    ((pair? prog) (append (blocked-intervals (car prog))
                    (map (bump-interval (code-length (list (car prog))))
                         (blocked-intervals (cdr prog)))))
    (else '())))

(define (ignore-blocked-registers availability interval blocked)
  "Remove blocked registers from the availability list"
  (apply assq-remove availability ((overlap-interval blocked) interval)))

(define (blocked-predefined predefined intervals blocked)
  "Get blocked predefined registers"
  (filter
    (lambda (pair)
      (let [(variable (car pair))
            (register (cdr pair))]
        (and (assq-ref intervals variable)
             (memv register ((overlap-interval blocked) (assq-ref intervals variable))))))
    predefined))

(define (move-blocked-predefined blocked-predefined)
  "Generate code for blocked predefined variables"
  (map (compose MOV car+cdr) blocked-predefined))

(define (non-blocked-predefined predefined blocked-predefined)
  "Compute the set difference of the predefined variables and the variables with blocked registers"
  (difference predefined blocked-predefined))

(define (linear-scan-coloring live-intervals registers predefined blocked)
  "Linear scan register allocation based on live intervals"
  (define (linear-allocate live-intervals register-use variable-use allocation)
    (if (null? live-intervals)
        allocation
        (let* [(candidate    (car live-intervals))
               (variable     (car candidate))
               (interval     (cdr candidate))
               (first-index  (car interval))
               (last-index   (cdr interval))
               (variable-use (mark-used-till variable-use variable last-index))
               (availability (ignore-blocked-registers register-use interval blocked))
               (register     (or (assq-ref predefined variable)
                                 (find-available-register availability first-index)))
               (recursion    (lambda (allocation register)
                               (linear-allocate (cdr live-intervals)
                                                (mark-used-till register-use register last-index)
                                                variable-use
                                                (assq-set allocation variable register))))]
          (if register
            (recursion allocation register)
            (let* [(spill-targets (ignore-spilled-variables variable-use allocation))
                   (target        (spill-candidate spill-targets))
                   (register      (assq-ref allocation target))]
              (recursion (assq-set allocation target #f) register))))))
  (linear-allocate (sort-live-intervals live-intervals (map car predefined))
                   (initial-register-use registers)
                   '()
                   '()))
