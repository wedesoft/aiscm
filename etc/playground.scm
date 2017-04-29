(use-modules (oop goops)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-64)
             (system foreign)
             (aiscm element)
             (aiscm int)
             (aiscm sequence)
             (aiscm mem)
             (aiscm pointer)
             (aiscm rgb)
             (aiscm complex)
             (aiscm obj)
             (aiscm asm)
             (aiscm jit)
             (aiscm method)
             (aiscm util)
             (aiscm tensor))

(test-begin "playground")

; (tensor (dim i (+ (get (seq 2 3 5) i) 1)))

(define ctx (make <context>))

; (jit ctx (list (sequence <ubyte>) <ubyte>) (lambda (a b) (dim i (+ (get a i) b))))

(define context ctx)
(define classes (list (sequence <ubyte>) <ubyte>))
(define proc (lambda (a b) (dim i (+ (get a i) b))))

(define vars         (map skeleton classes))
(define expr         (apply proc (map parameter vars)))
(define result-type  (type expr))
(define result       (parameter result-type))
(define types        (map class-of vars))

;(define intermediate (generate-return-code vars result expr))
(define args vars)
(define expr expr)
(define intermediate result)

;(code intermediate expr)
(tensor-loop expr)

;(define instructions (asm context
;                         <ulong>
;                  (map typecode (content-vars vars))
;                  (apply virtual-variables (apply assemble intermediate))))
;(define fun          (lambda header (apply instructions (append-map unbuild types header))))

(test-end "playground")
