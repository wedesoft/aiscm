(define-module (aiscm op)
  #:use-module (oop goops)
  #:use-module (ice-9 curried-definitions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (aiscm util)
  #:use-module (aiscm asm)
  #:use-module (aiscm jit)
  #:use-module (aiscm mem)
  #:use-module (aiscm element)
  #:use-module (aiscm pointer)
  #:use-module (aiscm bool)
  #:use-module (aiscm int)
  #:use-module (aiscm rgb)
  #:use-module (aiscm complex)
  #:use-module (aiscm sequence)
  #:export (fill)
  #:re-export (+ - * / % = < <= > >= min max))
(define ctx (make <context>)); TODO: remove this

(define-method (to-type (target <meta<element>>) (self <sequence<>>))
  (let [(proc (let [(fun (jit ctx (list (class-of self)) (cut to-type target <>)))]
                (lambda (target self) (fun self))))]
    (add-method! to-type
                 (make <method>
                       #:specializers (list (class-of target) (class-of self))
                       #:procedure proc))
    (to-type target self)))

(define (fill type shape value)
  (let [(retval (make (multiarray type (length shape)) #:shape shape))]
    (store retval value)
    retval))
