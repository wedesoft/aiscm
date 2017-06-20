(use-modules (oop goops) (aiscm jit) (aiscm pointer) (aiscm int) (aiscm asm) (aiscm expression) (aiscm obj) (aiscm sequence) (aiscm element) (srfi srfi-1) (aiscm operation) (aiscm rgb) (aiscm tensor) (aiscm variable))

(define s (parameter (sequence <int>)))
(define i (var <long>))
(define v (parameter <int>))
(code v (injecter += i (get s i)))

(tensor (sum i (get (seq <obj> 2 3 5) i)))
(tensor-body (sum i (get (seq <obj> 2 3 5) i)))

(tensor-variables '(sum i (get (seq <obj> 2 3 5) i)))

(define ctx (make <context>))
(define context ctx)
(define classes (list <intrgb>))
(define proc identity)

(define args         (map skeleton classes))
(define parameters   (map parameter args))
(define expr         (apply proc parameters))
(define result-type  (type expr))
(define intermediate (parameter result-type))

(define retval (skeleton <intrgb>))

(define out (parameter retval))

(code out scm-eol)

(code intermediate expr)

(delegate intermediate)
(delegate expr)

(code out (package-return-content intermediate))

(define result       (generate-return-code args intermediate expr))
(define commands     (apply virtual-variables (apply assemble result)))
(define instructions (asm context <ulong> (map typecode (content-vars args)) commands))
(define fun          (lambda header (apply instructions (append-map unbuild classes header))))

((jit ctx (list <intrgb>) identity) (rgb 2 3 5))
