(use-modules (oop goops)
             (srfi srfi-1)
             (srfi srfi-64)
             (aiscm element)
             (aiscm int)
             (aiscm sequence)
             (aiscm util)
             (aiscm asm)
             (aiscm jit)
             (aiscm tensor))

(test-begin "playground")

(define-class <injecter> (<param>)
  (name  #:init-keyword #:name  #:getter name)
  (index #:init-keyword #:index #:getter index))

(define-method (type (self <injecter>))
  (type (delegate self)))

(define (injecter name index delegate)
  (make <injecter> #:name name #:index index #:delegate delegate))

(define-syntax-rule (inject name index delegate)
  (let [(index (var <long>))]
    (injecter name index delegate)))

(define-method (code (out <param>) (in <injecter>))
  (list (MOV (get (delegate out)) 10)))

(test-begin "tensor reduce")
  (let [(s (parameter (sequence <ubyte>)))
        (m (parameter (multiarray <ubyte> 2)))]
    (test-equal "\"inject\" reduces 1D array to scalar"
      <ubyte> (type (inject + i (get s i))))
    (test-equal "\"inject\" reduces 2D array to 1D"
      (sequence <ubyte>) (type (inject + i (get m i))))
    (test-eqv "Sum elements using tensor expression"
      10 (tensor (inject + k (get (seq 2 3 5) k)))))
(test-end "tensor reduce")

;(define i (var <long>))
;(define s (parameter (sequence <ubyte>)))
;
;(define expr '(inject + i (get (seq 2 3 5) i)))
;
;(tensor-operations expr)
;(tensor-variables expr)
;
;(expression->identifier expr)
;
;(define retval (parameter <ubyte>))
;
;(code retval expr)
;
;(define ctx (make <context>))
;
;((jit ctx (list (sequence <ubyte>)) (lambda (s) (inject + i (get s i)))) (seq 2 3 5))

(test-end "playground")
