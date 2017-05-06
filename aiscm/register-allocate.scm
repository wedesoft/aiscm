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
(define-module (aiscm register-allocate)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (aiscm util)
  #:use-module (aiscm asm)
  #:use-module (aiscm variable)
  #:use-module (aiscm command)
  #:use-module (aiscm program)
  #:export (initial-register-use sort-live-intervals find-available-register mark-used-till
            spill-candidate ignore-spilled-variables ignore-blocked-registers next-indices live-analysis))

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

(define (ignore-blocked-registers availability interval blocked)
  "Remove blocked registers from the availability list"
  (apply assq-remove availability ((overlap-interval blocked) interval)))

(define-method (next-indices labels cmd k)
  "Determine next program indices for a statement"
  (if (equal? cmd (RET)) '() (list (1+ k))))
(define-method (next-indices labels (cmd <jcc>) k)
  "Determine next program indices for a (conditional) jump"
  (let [(target (assq-ref labels (get-target cmd)))]
    (if (conditional? cmd) (list (1+ k) target) (list target))))

(define (live-analysis prog results)
  "Get list of live variables for program terminated by RET statement"
  (letrec* [(inputs    (map-if (cut equal? (RET) <>) (const results) input prog))
            (outputs   (map output prog))
            (indices   (iota (length prog)))
            (lut       (labels prog))
            (flow      (map (cut next-indices lut <...>) prog indices))
            (same?     (cut every (cut lset= equal? <...>) <...>))
            (track     (lambda (value)
                         (lambda (in ind out)
                           (union in (difference (apply union (map (cut list-ref value <>) ind)) out)))))
            (initial   (map (const '()) prog))
            (iteration (lambda (value) (map (track value) inputs flow outputs)))]
    (map union (fixed-point initial iteration same?) outputs)))
