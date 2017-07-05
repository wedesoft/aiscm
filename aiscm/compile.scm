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
(define-module (aiscm compile)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 curried-definitions)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm asm)
  #:use-module (aiscm util)
  #:use-module (aiscm command)
  #:use-module (aiscm variable)
  #:use-module (aiscm program)
  #:use-module (aiscm live-analysis)
  #:use-module (aiscm register-allocate)
  #:export (replace-variables adjust-stack-pointer
            default-registers callee-saved caller-saved parameter-registers register-parameters stack-parameters
            register-parameter-locations stack-parameter-locations parameter-locations add-stack-parameter-information
            used-callee-saved temporary-variables unit-intervals temporary-registers
            need-to-copy-first move-variable-content update-parameter-locations
            place-result-variable backup-registers jit-compile))


(define (replace-variables allocation cmd temporaries)
  "Substitute variables with registers and add spill code using temporary registers if necessary"
  (let* [(primary-var (first-argument cmd))
         (primary-loc (assq-ref allocation primary-var))]
    (if (is-a? primary-loc <address>)
      (let* [(register   (car temporaries))
             (temporary  (to-type (typecode primary-var) register))
             (is-input?  (memv primary-var (input cmd)))
             (is-output? (memv primary-var (output cmd)))]
        (filter identity
          (cons (and is-input? (MOV temporary primary-loc))
                (attach (replace-variables allocation
                                           (substitute-variables cmd (list (cons primary-var register)))
                                           (cdr temporaries))
                        (and is-output? (MOV primary-loc temporary))))))
      (let [(spilled-pointers (filter (lambda (arg) (is-a? (assq-ref allocation arg) <address>)) (get-ptr-args cmd)))]
        (attach (map (lambda (var temporary) (MOV temporary (assq-ref allocation var))) spilled-pointers temporaries)
                (substitute-variables cmd (fold (lambda (var tmp alist) (assq-set alist var tmp))
                                                allocation spilled-pointers temporaries)))))))

(define (adjust-stack-pointer offset prog)
  "Adjust stack pointer offset at beginning and end of program"
  (append (list (SUB RSP offset)) (all-but-last prog) (list (ADD RSP offset) (RET))))

;RSP is not included because it is used as a stack pointer
(define default-registers (list RAX RCX RDX RSI RDI R10 R11 R9 R8 R12 R13 R14 R15 RBX RBP))
(define callee-saved (list RBX RBP RSP R12 R13 R14 R15))
(define caller-saved (list RAX RCX RDX RSI RDI R10 R11 R9 R8))
(define parameter-registers (list RDI RSI RDX RCX R8 R9))

(define (register-parameters parameters)
   "Return the parameters which are stored in registers according to the x86 ABI"
   (take-up-to parameters 6))

(define (stack-parameters parameters)
   "Return the parameters which are stored on the stack according to the x86 ABI"
   (drop-up-to parameters 6))

(define (register-parameter-locations parameters)
  "Create an association list with the initial parameter locations"
  (map cons parameters parameter-registers))

(define (stack-parameter-locations parameters offset)
  "Determine initial locations of stack parameters"
  (map (lambda (parameter index) (cons parameter (ptr <long> RSP index)))
       parameters
       (iota (length parameters) (+ 8 offset) 8)))

(define (parameter-locations parameters offset)
  "return association list with default locations for the method parameters"
  (let [(register-parameters (register-parameters parameters))
        (stack-parameters    (stack-parameters parameters))]
    (append (register-parameter-locations register-parameters)
            (stack-parameter-locations stack-parameters offset))))

(define (add-stack-parameter-information allocation stack-parameter-locations)
   "Add the stack location for stack parameters which do not have a register allocated"
   (map (lambda (variable location) (cons variable (or location (assq-ref stack-parameter-locations variable))))
        (map car allocation)
        (map cdr allocation)))

(define (used-callee-saved allocation)
   "Return the list of callee saved registers in use"
   (delete-duplicates (lset-intersection eq? (apply compact (map cdr allocation)) callee-saved)))

(define (temporary-variables cmd)
  "Allocate temporary variable for each instruction which has a variable as first argument"
   (let [(arg (first-argument cmd))]
     (cond
       ((is-a? arg <ptr>)                (list (var <long>) (var <long>)))
       ((not (null? (get-ptr-args cmd))) (list (var <long>)))
       ((is-a? arg <var>)                (list (var <long>)))
       (else                             '()))))

(define (unit-intervals temporaries)
  "Generate intervals of length one for each temporary variable"
  (append-map (lambda (vars index) (map (cut cons <> (cons index index)) vars))
              temporaries
              (iota (length temporaries))))

(define ((temporary-registers allocation) variables)
  "Look up register for each temporary variable given the result of a register allocation"
  (map (cut assq-ref allocation <>) variables))

(define (need-to-copy-first initial targets a b)
  "Check whether parameter A needs to be copied before B given INITIAL and TARGETS locations"
  (eq? (assq-ref initial a) (assq-ref targets b)))

(define (move-variable-content variable source destination)
  "move VARIABLE content from SOURCE to DESTINATION unless source and destination are the same"
  (let [(adapt (cut to-type (typecode variable) <>))]
    (if (or (not destination) (equal? source destination)) '() (MOV (adapt destination) (adapt source)))))

(define (update-parameter-locations parameters locations offset)
  "Generate the required code to update the parameter locations according to the register allocation"
  (let* [(initial            (parameter-locations parameters offset))
         (ordered-parameters (partial-sort parameters (cut need-to-copy-first initial locations <...>)))]
    (filter (compose not null?)
      (map (lambda (parameter)
             (move-variable-content parameter
                                    (assq-ref initial parameter)
                                    (assq-ref locations parameter)))
           ordered-parameters))))

(define (place-result-variable results locations code)
  "add code for placing result variable in register RAX if required"
  (filter (compose not null?)
          (attach (append (all-but-last code)
                          (map (lambda (result) (move-variable-content result (assq-ref locations result) RAX)) results))
                  (RET))))

(define (backup-registers registers code)
  "Store register content on stack and restore it after executing the code"
  (append (map (cut PUSH <>) registers) (all-but-last code) (map (cut POP <>) (reverse registers)) (list (RET))))

(define* (jit-compile prog #:key (registers default-registers) (parameters '()) (blocked '()) (results '()))
  "Linear scan register allocation for a given program"
  (let* [(live                 (live-analysis prog results))
         (temp-vars            (map temporary-variables prog))
         (intervals            (append (live-intervals live (variables prog))
                                       (unit-intervals temp-vars)))
         (predefined-registers (register-parameter-locations (register-parameters parameters)))
         (parameters-to-move   (blocked-predefined predefined-registers intervals blocked))
         (remaining-predefines (non-blocked-predefined predefined-registers parameters-to-move))
         (stack-parameters     (stack-parameters parameters))
         (colors               (linear-scan-coloring intervals registers remaining-predefines blocked))
         (callee-saved         (used-callee-saved colors))
         (stack-offset         (* 8 (1+ (number-spilled-variables colors stack-parameters))))
         (parameter-offset     (+ stack-offset (* 8 (length callee-saved))))
         (stack-locations      (stack-parameter-locations stack-parameters parameter-offset))
         (allocation           (add-stack-parameter-information colors stack-locations))
         (temporaries          (map (temporary-registers allocation) temp-vars))
         (locations            (add-spill-information allocation 8 8))]
    (backup-registers callee-saved
      (adjust-stack-pointer stack-offset
        (place-result-variable results locations
          (append (update-parameter-locations parameters locations parameter-offset)
                  (append-map (cut replace-variables locations <...>) prog temporaries)))))))
