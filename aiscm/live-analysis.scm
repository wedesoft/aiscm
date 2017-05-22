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
(define-module (aiscm live-analysis)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (aiscm util)
  #:use-module (aiscm asm)
  #:use-module (aiscm command)
  #:use-module (aiscm program)
  #:export (next-indices live-analysis flow))


(define-method (next-indices labels cmd k)
  "Determine next program indices for a statement"
  (if (equal? cmd (RET)) '() (list (1+ k))))
(define-method (next-indices labels (cmd <jcc>) k)
  "Determine next program indices for a (conditional) jump"
  (let [(target (assq-ref labels (get-target cmd)))]
    (if (conditional? cmd) (list (1+ k) target) (list target))))

(define (flow prog)
  (let [(labels (labels prog))]
    (map (cut next-indices labels <...>) prog (iota (length prog)))))

(define (live-analysis prog results)
  "Get list of live variables for program terminated by RET statement"
  (let* [(inputs    (inputs prog results))
         (outputs   (outputs prog))
         (flow      (flow prog))
         (same?     (cut every (cut lset= equal? <...>) <...>))
         (track     (lambda (value)
                      (lambda (input indices output)
                        (union input (difference (apply union (map (cut list-ref value <>) indices)) output)))))
         (initial   inputs)
         (iteration (lambda (value) (map (track value) inputs flow outputs)))]
    (map union (fixed-point initial iteration same?) outputs)))
