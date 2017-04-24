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
             (aiscm util))

(test-begin "playground")

(define ctx (make <context>))

(define (tensor-operations expr)
  "Check whether expression is a tensor operation"
  (define (argument-mask expr . indices)
    (map (lambda (idx) (and (memv idx indices) #t)) (iota (length expr))))
  (and (list? expr)
       (case (car expr)
         ((+)      (argument-mask expr 0))
         ((-)      (argument-mask expr 0))
         ((get)    (argument-mask expr 0 2))
         ((tensor) (argument-mask expr 0 1))
         (else #f))))

(define (expression->identifier expr)
  "Extract structure of tensor and convert to identifier"
  (let [(mask (tensor-operations expr))]
    (if mask (map-select mask identity expression->identifier expr) '_)))

(define (identifier->symbol identifier)
  "Convert identifier to a symbol which can be used as a method name"
  (string->symbol (call-with-output-string (cut write identifier <>))))

(define (tensor-variables expr)
  "Return variables of tensor expression"
  (let [(mask (tensor-operations expr))]
    (if mask
        (concatenate (map-select mask (const '()) tensor-variables expr))
        (list expr))))

(define (consume-variables mask identifier variables)
  "Build arguments of expression and return remaining variables"
  (if (null? identifier)
    (cons identifier variables)
    (let* [(head (if (car mask)
                     (cons (car identifier) variables)
                     (build-expression (car identifier) variables)))
           (tail (consume-variables (cdr mask) (cdr identifier) (cdr head)))]
      (cons (cons (car head) (car tail)) (cdr tail)))))

(define (build-expression identifier variables)
  "Build a tensor expression and return remaining variables"
  (let [(mask (tensor-operations identifier))]
    (if mask
        (consume-variables mask identifier variables)
        variables)))

(define (identifier->expression identifier variables)
  "Convert identifier to tensor expression with variables"
  (car (build-expression identifier variables)))

;(define-macro (xxx expr)
;  (let* [(vars       (tensor-variables expr))
;         (args       (symbol-list (length vars)))
;         (identifier (expression->identifier expr))
;         (name       (identifier->symbol identifier))
;         (prog       (identifier->expression identifier args))]
;    `(begin
;       (if (not (defined? (quote ,name) (current-module)))
;         (define-method (,name . ,args)
;           (let [(f (jit ctx (map class-of (list . ,args)) (lambda ,args ,prog)))]
;             (add-method! ,name
;                          (make <method>
;                                #:specializers (map class-of (list . ,args))
;                                #:procedure (lambda args (apply f args))))
;             (,name . ,vars))))
;       (,name . ,vars))))

(define-macro (xxx expr)
  "Instantiate a compiled tensor expression"
  (let* [(vars       (tensor-variables expr))
         (identifier (expression->identifier expr))
         (args       (symbol-list (length vars)))
         (name       (identifier->symbol identifier))
         (prog       (identifier->expression identifier args))]
    `(begin
      (if (not (defined? (quote ,name) (current-module)))
        (define-method (,name . ,args)
          (let [(f (jit ctx (map class-of (list . ,args)) (lambda ,args ,prog)))]
            (add-method! ,name (make <method>
                                     #:specializers (map class-of (list . ,args))
                                     #:procedure f)))
          (,name . ,vars)))
      (,name . ,vars))))

(define (f a b) (xxx (- (* 2 a) b)))
(f (seq 2 3 5) (seq 3 5 7))

;((jit ctx (list (sequence <ubyte>)) (lambda (s) (tensor k (get s k)))) (seq 2 3 5))

(test-begin "identify tensor operations")
  (test-equal "+ is a tensor operation"
    '(#t #f #f) (tensor-operations '(+ x y)))
  (test-equal "- is a tensor operation"
    '(#t #f) (tensor-operations '(- x)))
  (test-assert "x is not a tensor operation"
    (not (tensor-operations 'x)))
  (test-assert "\"read-image\" is not a tensor operation"
    (not (tensor-operations '(read-image "test.bmp"))))
  (test-equal "\"get\" is a tensor operation"
    '(#t #f #t) (tensor-operations '(get s k)))
  (test-equal "\"tensor\" is a tensor operation"
    '(#t #t #f) (tensor-operations '(tensor k s)))
(test-end "identify tensor operations")

(test-begin "convert tensor expression to identifier")
  (test-equal "filter variable names in expression"
    '_ (expression->identifier 'x))
  (test-equal "filter numeric arguments of expression"
    '_ (expression->identifier 42))
  (test-equal "preserve unary plus operation"
    '(+ _) (expression->identifier '(+ x)))
  (test-equal "preserve unary minus operation"
    '(- _) (expression->identifier '(- x)))
  (test-equal "preserve binary plus operation"
    '(+ _ _) (expression->identifier '(+ x y)))
  (test-equal "works recursively"
    '(+ (- _) _) (expression->identifier '(+ (- x) y)))
  (test-equal "filter non-tensor operations"
    '_ (expression->identifier '(read-image "test.bmp")))
  (test-equal "preverse tensor index when accessing array"
    '(get _ k) (expression->identifier '(get s k)))
(test-end "convert tensor expression to identifier")

(test-begin "convert identifier to symbol")
  (test-equal "simple symbol stays same"
    '_ (identifier->symbol '_))
  (test-equal "expression is converted to symbol"
    "(- _)" (symbol->string (identifier->symbol '(- _))))
(test-end "convert identifier to symbol")

(test-begin "extract variables of tensor expression")
  (test-equal "detect variable name"
    '(x) (tensor-variables 'x))
  (test-equal "detect numerical arguments"
    '(42) (tensor-variables 42))
  (test-equal "extract argument of unary plus"
    '(x) (tensor-variables '(+ x)))
  (test-equal "extract argument of unary minus"
    '(x) (tensor-variables '(- x)))
  (test-equal "extract arguments of binary plus"
    '(x y) (tensor-variables '(+ x y)))
  (test-equal "extract variables recursively"
    '(x y) (tensor-variables '(+ (- x) y)))
  (test-equal "extract non-tensor operations"
    '((read-image "test.bmp")) (tensor-variables '(read-image "test.bmp")))
  (test-equal "extract variables of tensor index access"
    '(s) (tensor-variables '(get s k)))
(test-end "extract variables of tensor expression")

(test-begin "convert tensor identifier to tensor expression")
  (test-equal "single variable expression"
    (cons 'x '(y z)) (build-expression '_ '(x y z)))
  (test-equal "reconstruct unary plus operation"
    (cons '(+ x) '(y)) (build-expression '(+ _) '(x y)))
  (test-equal "build arguments of unary expression"
    (cons '(x) '(y z)) (consume-variables '(#f) '(_) '(x y z)))
  (test-equal "build arguments of binary expression"
    (cons '(x y) '(z)) (consume-variables '(#f #f) '(_ _) '(x y z)))
  (test-equal "apply argument mask when consuming variables"
    (cons '(+ x y) '(z)) (consume-variables '(#t #f #f) '(+ _ _) '(x y z)))
  (test-equal "build binary expression"
    (cons '(+ x y) '(z)) (build-expression '(+ _ _) '(x y z)))
  (test-equal "consume variables recursively"
    (cons '((- x)) '(y z)) (consume-variables '(#f) '((- _)) '(x y z)))
  (test-equal "reconstruct single variable expression"
    'x (identifier->expression '_ '(x)))
  (test-equal "reconstruct expression"
    '(- x y) (identifier->expression '(- _ _) '(x y)))
  (test-equal "reconstruct nested expressions"
    '(+ (- x) y) (identifier->expression '(+ (- _) _) '(x y)))
  (test-equal "insert non-tensor operations"
    '(read-image "test.bmp") (identifier->expression '_ '((read-image "test.bmp"))))
  (test-equal "reconstruct tensor index access"
    '(get s k) (identifier->expression '(get _ k) '(s)))
(test-end "convert tensor identifier to tensor expression")

(test-begin "tensor macro")
  (test-equal "test trivial tensor"
    '(2 3 5) (to-list (xxx (seq 2 3 5))))
  (xxx (seq 2 3 5))
  (test-assert "tensor macro defines a generic for compiled methods"
    (defined? '_ (current-module)))
  (test-equal "trivial tensor operation is cached using a generic"
    '(2 3 5) (to-list (_ (seq 2 3 5))))
  (test-equal "cached operation uses provided values"
    '(3 5 7) (to-list (_ (seq 3 5 7))))
  (define num-generics (length (generic-function-methods _)))
  (xxx (seq <int> 2 3 5))
  (test-eqv "tensor operations are cached for each type"
    (1+ num-generics) (length (generic-function-methods _)))
  (test-equal "test tensor sum"
    '(5 8 12) (to-list (xxx (+ (seq 2 3 5) (seq 3 5 7)))))
  (xxx (+ (seq 2 3 5) (seq 3 5 7)))
  (test-equal "tensor sum is cached using a generic"
    '(5 8 12) (to-list ((eval (string->symbol "(+ _ _)") (current-module)) (seq 2 3 5) (seq 3 5 7))))
  (test-equal "using tensor macro with indexing"
    '(2 3 5) (to-list (xxx (tensor k (get (seq 2 3 5) k)))))
  (test-equal "use tensor macro to transpose matrix"
    '((2 3) (3 5) (5 7)) (to-list (xxx (tensor j (tensor i (get (get (arr (2 3 5) (3 5 7)) i) j))))))
(test-end "tensor macro")

(test-end "playground")
