;(use-modules (srfi srfi-64))
(use-modules (oop goops) (srfi srfi-1) (srfi srfi-26) (aiscm asm) (aiscm int) (aiscm rgb) (aiscm expression) (aiscm jit) (aiscm operation) (aiscm element) (aiscm sequence) (aiscm scalar) (ice-9 curried-definitions) (aiscm composite) (aiscm tensor) (aiscm variable) (aiscm loop) (aiscm pointer))

(tensor (sum i (get (seq (rgb 1 2 3) (rgb 2 3 4)) i)))

(define s (parameter (sequence <ubytergb>)))
(define i (var <long>))
(get s i)
(define b (injecter += i (get s i)))
(define a (parameter <ubytergb>))
(duplicate a b)

(define t (multi-loop (delegate b) (index b)))
(define tmp (parameter (typecode a)))
(duplicate tmp (body t))

(+= tmp (body t)); TODO: handle pointers

(define r tmp)
(define a tmp)
(define b (body t))

((+= <ubytergb> <ubytergb>) r a b)

(define intermediate b)
(define ta (type a))
(define tb (type b))

(content <rgb<>> b); !!!
;(define-method (content (type <meta<rgb<>>>) (self <param>))
;  (content type (decompose-value type self)))

(deconstruct <rgb<>> (delegate b))

(define ctx (make <context>))
(define i (var <long>))
((jit ctx (list (sequence <ubytergb>)) (lambda (s) (injecter += i (get s i)))) (seq (rgb 2 3 5)))


