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
(define-module (aiscm operation)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 curried-definitions)
  #:use-module (aiscm element)
  #:use-module (aiscm bool)
  #:use-module (aiscm int)
  #:use-module (aiscm pointer)
  #:use-module (aiscm sequence)
  #:use-module (aiscm asm)
  #:use-module (aiscm command)
  #:use-module (aiscm expression)
  #:use-module (aiscm loop)
  #:use-module (aiscm method)
  #:use-module (aiscm util)
  #:export (make-constant-function native-const need-conversion? code
            code-needs-intermediate? operand code force-parameters mutating-code functional-code unary-extract)
  #:re-export (size-of -)
  #:export-syntax (define-operator-mapping let-skeleton let-parameter))

(define* ((native-data native) out args)
  (list (MOV (get (delegate out)) (get native))))
(define (make-constant-function native . args)
  (make-function make-constant-function (const (return-type native)) (native-data native) args))
(define (native-const type value)
  (make-constant-function (native-value type value)))

(define-method (operand (a <element>)) (get a))
(define-method (operand (a <pointer<>>))
  (if (pointer-offset a)
      (ptr (typecode a) (get a) (pointer-offset a))
      (ptr (typecode a) (get a))))
(define-method (operand (a <param>)) (operand (delegate a)))

(define-method (need-conversion? target type) (not (eq? target type)))
(define-method (need-conversion? (target <meta<int<>>>) (type <meta<int<>>>))
  (not (eqv? (size-of target) (size-of type))))
(define-method (need-conversion? (target <meta<bool>>) (type <meta<int<>>>))
  (not (eqv? (size-of target) (size-of type))))
(define-method (need-conversion? (target <meta<int<>>>) (type <meta<bool>>))
  (not (eqv? (size-of target) (size-of type))))

(define-method (force-parameters (targets <list>) args predicate fun)
  (let* [(mask          (map predicate targets args))
         (intermediates (map-select mask (compose parameter car list) (compose cadr list) targets args))
         (preamble      (concatenate (map-select mask code (const '()) intermediates args)))]
    (attach preamble (apply fun intermediates))))
(define-method (force-parameters target args predicate fun)
  (force-parameters (make-list (length args) target) args predicate fun))

(define-syntax-rule (let-prototype [(name prototype value)] body ...)
  (let [(name prototype)] (list (code name value) body ...)))
(define-syntax-rule (let-skeleton [(name type value)] body ...)
  (let-prototype [(name (skeleton type) value)] body ...))
(define-syntax-rule (let-parameter [(name type value)] body ...)
  (let-prototype [(name (parameter type) value)] body ...))

(define-method (code (a <element>) (b <element>)) ((to-type (typecode a) (typecode b)) (parameter a) (list (parameter b))))
(define-method (code (a <element>) (b <integer>)) (list (MOV (operand a) b)))
(define-method (code (a <pointer<>>) (b <pointer<>>))
  (let-skeleton [(tmp (typecode a) b)] (code a tmp)))
(define-method (code (a <param>) (b <param>)) (code (delegate a) (delegate b)))
(define-method (code (a <indexer>) (b <param>))
  (let [(dest   (multi-loop a))
        (source (multi-loop b))]
    (append (append-map loop-setup (loop-details dest))
            (append-map loop-setup (loop-details source))
            (repeat 0
                    (value (dimension a))
                    (code (body dest) (body source))
                    (append-map loop-increment (loop-details dest))
                    (append-map loop-increment (loop-details source))))))
(define-method (code (out <element>) (fun <function>))
  (if (need-conversion? (typecode out) (type fun))
    (let-skeleton [(tmp (type fun) fun)] (code out tmp))
    ((term fun) (parameter out))))
(define-method (code (out <pointer<>>) (fun <function>))
  (let-skeleton [(tmp (typecode out) fun)] (code out tmp)))
(define-method (code (out <param>) (fun <function>)) (code (delegate out) fun))
(define-method (code (out <param>) (value <integer>)) (code out (native-const (type out) value)))
(define-method (code (a <param>) (b <injecter>))
  (let [(t (multi-loop (delegate b) (index b)))]
    (append
      (append-map loop-setup (loop-details t))
      (let-parameter [(tmp (typecode a) (body t))]
         (append-map loop-increment (loop-details t))
         (repeat 1 (value (dimension-hint (index b)))
                 ((name b) tmp (body t)); TODO: composite values
                 (append-map loop-increment (loop-details t)))
         (code a tmp)))))

(define-method (size-of (self <param>))
  (apply * (native-const <long> (size-of (typecode (type self)))) (shape self)))

(define (code-needs-intermediate? t value) (or (is-a? value <function>) (need-conversion? t (type value))))

(define (operation-code target op out args)
  "Adapter for nested expressions"
  (force-parameters target args code-needs-intermediate?
    (lambda intermediates
      (apply op (operand out) (map operand intermediates)))))
(define ((mutating-code op) out args)
  "Adapter for machine code overwriting its first argument"
  (append (code out (car args))
          (operation-code (type out) op out (cdr args))))
(define ((functional-code op) out args)
  "Adapter for machine code without side effects on its arguments"
  (operation-code (reduce coerce #f (map type args)) op out args))
(define ((unary-extract op) out args)
  "Adapter for machine code to extract part of a composite value"
  (code (delegate out) (apply op (map delegate args))))


(define-macro (define-operator-mapping name arity type fun)
  (let [(header (typed-header (symbol-list arity) type))]
    `(define-method (,name ,@header) ,fun)))

(define-operator-mapping -   1 <meta<int<>>> (mutating-code   NEG              ))
(define-method (- (z <integer>) (a <meta<int<>>>)) (mutating-code NEG))
