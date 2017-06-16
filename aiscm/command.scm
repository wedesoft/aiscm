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
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (aiscm asm)
  #:use-module (aiscm element)
  #:use-module (aiscm bool)
  #:use-module (aiscm int)
  #:use-module (aiscm variable)
  #:use-module (aiscm util)
  #:export (<cmd> <block>
            get-op get-ptr-args input output first-argument mov-signed mov-unsigned mov
            blocked sign-extend-ax div mod shl shr test-zero test-non-zero bool-and bool-or
            cmp cmp-equal cmp-not-equal cmp-lower-than cmp-lower-equal cmp-greater-than cmp-greater-equal
            minor major cmp-where cmp-abs repeat each-element)
  #:re-export (variables get-args get-reg get-code))

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

(define-class <block> ()
  (reg  #:init-keyword #:reg  #:getter get-reg)
  (code #:init-keyword #:code #:getter get-code))

(define-method (blocked (reg <register>) . body)
  "reserve a register so that the register allocator will not use it"
  (make <block> #:reg reg #:code body))
(define-method (blocked (lst <null>) . body)
  "reserve empty list of registers (no effect)"
  body)
(define-method (blocked (lst <pair>) . body)
  "reserve multiple registers so that the register allocator will not use them"
  (blocked (car lst) (apply blocked (cdr lst) body)))

(define (sign-extend-ax size) (case size ((1) (CBW)) ((2) (CWD)) ((4) (CDQ)) ((8) (CQO))))
(define (div/mod-prepare-signed r a)
  (list (MOV (to-type (typecode r) RAX) a) (sign-extend-ax (size-of r))))
(define (div/mod-prepare-unsigned r a)
  (if (eqv? 1 (size-of r)) (list (MOVZX AX a)) (list (MOV (to-type (typecode r) RAX) a) (MOV (to-type (typecode r) RDX) 0))))
(define (div/mod-signed r a b) (attach (div/mod-prepare-signed r a) (IDIV b)))
(define (div/mod-unsigned r a b) (attach (div/mod-prepare-unsigned r a) (DIV b)))
(define (div/mod-block-registers r . code) (blocked RAX (if (eqv? 1 (size-of r)) code (blocked RDX code))))
(define (div/mod r a b . finalise) (div/mod-block-registers r ((if (signed? r) div/mod-signed div/mod-unsigned) r a b) finalise))
(define (div r a b) (div/mod r a b (MOV r (to-type (typecode r) RAX))))
(define (mod r a b) (div/mod r a b (if (eqv? 1 (size-of r)) (list (MOV AL AH) (MOV r AL)) (MOV r DX))))

(define (shx r x shift-signed shift-unsigned)
  (blocked RCX (mov-unsigned CL x) ((if (signed? r) shift-signed shift-unsigned) r CL)))
(define (shl r x) (shx r x SAL SHL))
(define (shr r x) (shx r x SAR SHR))

(define-method (test (a <var>)) (list (TEST a a)))
(define-method (test (a <ptr>))
  (let [(intermediate (var (typecode a)))]
    (list (MOV intermediate a) (test intermediate))))
(define (test-zero r a) (attach (test a) (SETE r)))
(define (test-non-zero r a) (attach (test a) (SETNE r)))
(define ((binary-bool op) a b)
  (let [(intermediate (var <byte>))]
    (attach (append (test-non-zero a a) (test-non-zero intermediate b)) (op a intermediate))))
(define bool-and (binary-bool AND))
(define bool-or  (binary-bool OR))

(define-method (cmp a b) (list (CMP a b)))
(define-method (cmp (a <ptr>) (b <ptr>))
  (let [(intermediate (var (typecode a)))]
    (cons (MOV intermediate a) (cmp intermediate b))))
(define ((cmp-setxx set-signed set-unsigned) out a b)
  (let [(set (if (or (signed? a) (signed? b)) set-signed set-unsigned))]
    (attach (cmp a b) (set out))))
(define cmp-equal         (cmp-setxx SETE   SETE  ))
(define cmp-not-equal     (cmp-setxx SETNE  SETNE ))
(define cmp-lower-than    (cmp-setxx SETL   SETB  ))
(define cmp-lower-equal   (cmp-setxx SETLE  SETBE ))
(define cmp-greater-than  (cmp-setxx SETNLE SETNBE))
(define cmp-greater-equal (cmp-setxx SETNL  SETNB ))

(define ((cmp-cmovxx set-signed set-unsigned jmp-signed jmp-unsigned) a b)
  (if (eqv? 1 (size-of a))
    (append (cmp a b) (list ((if (signed? a) jmp-signed jmp-unsigned) 'skip)) (mov a b) (list 'skip))
    (append (cmp a b) (list ((if (signed? a) set-signed set-unsigned) a b)))))
(define minor (cmp-cmovxx CMOVNLE CMOVNBE JL   JB  ))
(define major (cmp-cmovxx CMOVL   CMOVB   JNLE JNBE))

(define (cmp-where out m a b)
  "Select value using boolean variable"
  (list (test m) (JE 'zero) (mov out a) (JMP 'finish) 'zero (mov out b) 'finish))

(define (cmp-abs out)
  "Compute absolute value of signed or unsigned integers"
  (if (signed? out) (list (cmp out 0) (JNLE 'skip) (NEG out) 'skip) '()))

(define (repeat start end . body)
  "Repeat loop"
  (let [(i (var (typecode end)))]
    (list (MOV i start) 'begin (CMP i (value end)) (JNL 'end) (INC i) body (JMP 'begin) 'end)))

(define (each-element iterator end step . body)
  (list 'begin (CMP (value iterator) (value end)) (JNL 'end) body (ADD (value iterator) (value step)) (JMP 'begin) 'end))
