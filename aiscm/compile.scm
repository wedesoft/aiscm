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
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm asm)
  #:use-module (aiscm util)
  #:use-module (aiscm command)
  #:use-module (aiscm program)
  #:export (replace-variables adjust-stack-pointer
            default-registers callee-saved caller-saved parameter-registers register-parameters stack-parameters
            register-parameter-locations stack-parameter-locations parameter-locations add-stack-parameter-information
            used-callee-saved))


(define (replace-variables allocation cmd temporary)
  "Replace variables with registers and add spill code if necessary"
  (let* [(location         (cut assq-ref allocation <>))
         (primary-argument (first-argument cmd))
         (primary-location (location primary-argument))]
    ; cases requiring more than one temporary variable are not handled at the moment
    (if (is-a? primary-location <address>)
      (let [(register (to-type (typecode primary-argument) temporary))]
        (compact (and (memv primary-argument (input cmd)) (MOV register primary-location))
                 (substitute-variables cmd (assq-set allocation primary-argument temporary))
                 (and (memv primary-argument (output cmd)) (MOV primary-location register))))
      (let [(spilled-pointer (filter (compose (cut is-a? <> <address>) location) (get-ptr-args cmd)))]
        ; assumption: (get-ptr-args cmd) only returns zero or one pointer argument requiring a temporary variable
        (attach (map (compose (cut MOV temporary <>) location) spilled-pointer)
                (substitute-variables cmd (fold (lambda (var alist) (assq-set alist var temporary)) allocation spilled-pointer)))))))

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
