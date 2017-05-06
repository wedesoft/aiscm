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
(define-module (aiscm command)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (aiscm asm)
  #:use-module (aiscm element)
  #:use-module (aiscm bool)
  #:use-module (aiscm int)
  #:use-module (aiscm variable)
  #:use-module (aiscm util)
  #:export (<cmd>
            get-op get-ptr-args input output first-argument mov-signed mov-unsigned mov)
  #:re-export (variables get-args))

(define-method (input self) '())
(define-method (output self) '())
(define-class <cmd> ()
  (op     #:init-keyword #:op     #:getter get-op)
  (args   #:init-keyword #:args   #:getter get-args)
  (input  #:init-keyword #:input  #:getter get-input)
  (output #:init-keyword #:output #:getter get-output))
(define-method (initialize (self <cmd>) initargs)
  (let-keywords initargs #f (op (out '()) (io '()) (in '()))
    (next-method self (list #:op     op
                            #:args   (append out io in)
                            #:input  (append io in)
                            #:output (append io out)))))
(define-method (write (self <cmd>) port)
  (write (cons (generic-function-name (get-op self)) (get-args self)) port))
(define-method (equal? (a <cmd>) (b <cmd>)) (equal? (object-slots a) (object-slots b)))

(define-method (input (self <cmd>))
  (delete-duplicates (variables (append (get-input self) (filter (cut is-a? <> <ptr>) (get-args self))))))
(define-method (output (self <cmd>)) (variables (get-output self)))

(define-method (variables (self <cmd>)) (variables (get-args self)))
(define-method (variables (self <list>)) (delete-duplicates (append-map variables self)))

(define (get-ptr-args cmd)
  "get variables used as a pointer in a command"
  (filter (cut is-a? <> <var>) (append-map get-args (filter (cut is-a? <> <ptr>) (get-args cmd)))))

(define-method (first-argument self)
   "Return false for compiled instructions"
   #f)
(define-method (first-argument (self <cmd>))
   "Get first argument of machine instruction"
   (car (get-args self)))

(define (mov-part a b) (MOV a (to-type (integer (* 8 (size-of a)) signed) b)))
(define (movzx32 a b) (MOV (to-type (integer (* 8 (size-of b))unsigned) a) b))
(define (mov-cmd movxx movxx32 a b)
  (cond
        ((eqv? (size-of a) (size-of b)) MOV)
        ((<    (size-of a) (size-of b)) mov-part)
        ((eqv? (size-of b) 4)           movxx32)
        (else                           movxx)))
(define-method (mov-signed   (a <operand>) (b <operand>)) ((mov-cmd MOVSX MOVSX   a b) a b))
(define-method (mov-unsigned (a <operand>) (b <operand>)) ((mov-cmd MOVZX movzx32 a b) a b))
(define (mov a b)
  (list ((if (or (eq? (typecode b) <bool>) (signed? b)) mov-signed mov-unsigned) a b)))

(define-syntax-rule (mutating-op op)
  (define-method (op . args) (make <cmd> #:op op #:io (list (car args)) #:in (cdr args))))
(define-syntax-rule (functional-op op)
  (define-method (op . args) (make <cmd> #:op op #:out (list (car args)) #:in (cdr args))))
(define-syntax-rule (state-setting-op op)
  (define-method (op . args) (make <cmd> #:op op #:in args)))
(define-syntax-rule (state-reading-op op)
  (define-method (op . args) (make <cmd> #:op op #:out args)))

(functional-op    mov-signed  )
(functional-op    mov-unsigned)
(functional-op    MOV         )
(functional-op    MOVSX       )
(functional-op    MOVZX       )
(functional-op    LEA         )
(mutating-op      SHL         )
(mutating-op      SHR         )
(mutating-op      SAL         )
(mutating-op      SAR         )
(state-setting-op PUSH        )
(state-reading-op POP         )
(mutating-op      NEG         )
(mutating-op      NOT         )
(mutating-op      AND         )
(mutating-op      OR          )
(mutating-op      XOR         )
(mutating-op      INC         )
(mutating-op      ADD         )
(mutating-op      SUB         )
(mutating-op      IMUL        )
(mutating-op      IDIV        )
(mutating-op      DIV         )
(state-setting-op CMP         )
(state-setting-op TEST        )
(state-reading-op SETB        )
(state-reading-op SETNB       )
(state-reading-op SETE        )
(state-reading-op SETNE       )
(state-reading-op SETBE       )
(state-reading-op SETNBE      )
(state-reading-op SETL        )
(state-reading-op SETNL       )
(state-reading-op SETLE       )
(state-reading-op SETNLE      )
(mutating-op      CMOVB       )
(mutating-op      CMOVNB      )
(mutating-op      CMOVE       )
(mutating-op      CMOVNE      )
(mutating-op      CMOVBE      )
(mutating-op      CMOVNBE     )
(mutating-op      CMOVL       )
(mutating-op      CMOVNL      )
(mutating-op      CMOVLE      )
(mutating-op      CMOVNLE     )
