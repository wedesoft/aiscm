(define-module (aiscm tensor)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (aiscm asm)
  #:use-module (aiscm variable)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm util)
  #:use-module (aiscm jit)
  #:export (<injecter>
            tensor-operations expression->identifier identifier->symbol tensor-variables
            build-expression consume-variables identifier->expression tensor-ctx
            injecter += *=)
  #:re-export (jit get wrap dim)
  #:export-syntax (inject tensor tensor-body sum prod))


(define tensor-ctx (make <context>))

(define-class <injecter> (<param>)
  (name  #:init-keyword #:name  #:getter name)
  (index #:init-keyword #:index #:getter index))

(define-method (type (self <injecter>))
  (type (delegate self)))

(define-method (shape (self <injecter>))
  (shape (delegate self)))

(define (injecter name index delegate)
  (make <injecter> #:name name #:index index #:delegate delegate))

(define-syntax-rule (inject name index delegate)
  (let [(index (var <long>))]
    (injecter name index delegate)))

(define-syntax-rule (define-tensor-operation name op fun)
  (begin
    (define-method (op (a <param>) (b <param>))
      (fun (value a) (ptr (typecode b) (value b))))
    (define-syntax-rule (name index delegate)
      (inject op index delegate))))

(define-tensor-operation sum  += ADD )
(define-tensor-operation prod *= IMUL)

(define-method (tensor-loop (self <injecter>) . idx)
  (let [(t (apply tensor-loop (delegate self) idx))]
    (make <tensor-loop> #:loop-details (loop-details t)
                        #:body (injecter (name self) (index self) (body t)))))

(define-method (code (a <param>) (b <injecter>))
  (let [(t (tensor-loop (delegate b) (index b)))]
    (append
      (append-map loop-setup (loop-details t))
      (insert-intermediate (body t) (parameter (typecode a))
        (lambda (intermediate)
          (append (append-map loop-increment (loop-details t))
                  (repeat 1 (value (dimension-hint (index b)))
                          ((name b) intermediate (body t)); TODO: non-pointers, intermediate results, composite values
                          ;(code intermediate ((name b) intermediate (body t))); TODO: remove unnecessary copying
                          (append-map loop-increment (loop-details t)))
                  (code a intermediate)))))))

(define (tensor-operations expr)
  "Check whether expression is a tensor operation"
  (define (argument-mask expr . indices)
    (map (lambda (idx) (and (memv idx indices) #t)) (iota (length expr))))
  (and (list? expr)
       (if (memv (car expr) operations)
           (argument-mask expr 0)
           (case (car expr)
             ((get)    (apply argument-mask expr 0 (iota (- (length expr) 2) 2)))
             ((dim)    (apply argument-mask expr (iota (- (length expr) 1))))
             ((inject) (argument-mask expr 0 1 2))
             ((sum)    (argument-mask expr 0 1))
             ((prod)   (argument-mask expr 0 1))
             (else #f)))))

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

(define-macro (tensor-body expr)
  "Instantiate a compiled tensor expression"
  (let* [(vars       (tensor-variables expr))
         (identifier (expression->identifier expr))
         (args       (symbol-list (length vars)))
         (name       (identifier->symbol identifier))
         (prog       (identifier->expression identifier args))]
    `(begin
      (if (not (defined? (quote ,name) (current-module)))
        (define-method (,name ,@args)
          (let [(fun (jit tensor-ctx (map class-of (list ,@args)) (lambda ,args ,prog)))]
            (add-method! ,name (make <method>
                                     #:specializers (map class-of (list ,@args))
                                     #:procedure (lambda args (apply fun (map get args))))))
          (apply ,name (map wrap (list ,@args)))))
      (apply ,name (map wrap (list ,@vars))))))

(define-macro (tensor . args)
  "Shortcut for tensor with indices"
  (let [(expr    (last args))
        (indices (all-but-last args))]
    (if (null? indices)
      `(tensor-body ,expr)
      `(tensor (dim ,(car indices) ,@(attach (cdr indices) expr))))))
