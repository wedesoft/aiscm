(use-modules (oop goops)
             (srfi srfi-1)
             (srfi srfi-26)
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

(define-method (project (self <injecter>))
  (injecter (name self) (index self) (project (delegate self))))

(define-method (project (self <injecter>) (idx <var>))
  (injecter (name self) (index self) (project (delegate self) idx)))

(define-method (tensor-loop (self <injecter>))
  (let [(t (tensor-loop (delegate self)))]
    (make <tensor-loop> #:loop-details (loop-details t)
                        #:body (injecter (name self) (index self) (body t)))))
; TODO: tensor-loop with index

(define-method (code (a <param>) (b <injecter>))
  (let [(t (tensor-loop (delegate b) (index b)))]
    (append
      (append-map loop-setup (loop-details t))
      (insert-intermediate (body t) (parameter (typecode a))
        (lambda (intermediate)
          (append (append-map loop-increment (loop-details t))
                  (repeat 1 (value (dimension-hint (index b)))
                          (code intermediate (+ intermediate (body t))); TODO: remove unnecessary copying
                          (append-map loop-increment (loop-details t)))
                  (code a intermediate)))))))

(define-method (code (a <indexer>) (b <injecter>))
  (let [(dest   (tensor-loop a))
        (source (tensor-loop b))]
    (append (append-map loop-setup (loop-details dest))
            (append-map loop-setup (loop-details source))
            (repeat 0
                    (value (dimension a))
                    (code (body dest) (body source))
                    (append-map loop-increment (loop-details dest))
                    (append-map loop-increment (loop-details source))))))

(test-begin "tensor reduce")
  (let [(s (parameter (sequence <ubyte>)))
        (m (parameter (multiarray <ubyte> 2)))]
    (test-equal "\"inject\" reduces 1D array to scalar"
      <ubyte> (type (inject + i (get s i))))
    (test-equal "\"inject\" reduces 2D array to 1D"
      (sequence <ubyte>) (type (inject + i (get m i))))
    (test-eqv "Sum elements using tensor expression"
      10 (tensor (inject + k (get (seq 2 3 5) k))))
    (test-eqv "Sum different elements using tensor expression"
      12 (tensor (inject + k (get (seq 2 3 7) k))))
    (test-eq "Project a tensor sum"
      <ubyte> (type (project (inject + i (get m i)))))
    (test-eq "Project a tensor sum by index"
      <ubyte> (type (project (dim j (inject + i (get m i j))))))
    (test-eq "typecode of array of integer sums is integer"
      <ubyte> (typecode (car (loop-details (tensor-loop (inject + i (get m i)))))))
    (test-eq "preserve injection when looping over array of sums"
      <injecter> (class-of (body (tensor-loop (inject + i (get m i))))))
    (test-equal "Tensor sum along one axis"
      '(4 6 8) (to-list (tensor (inject + k (get (arr (1 2 3) (3 4 5)) k))))))
(test-end "tensor reduce")

(define m (parameter (multiarray <ubyte> 2)))
(define expr (inject + k (get m k)))

(define ctx (make <context>))

(jit ctx (list (multiarray <ubyte> 2)) (lambda (m) (inject + i (get m i))))

(define context ctx)
(define classes (list (multiarray <ubyte> 2)))
(define proc (lambda (m) (inject + i (get m i))))

(define args         (map skeleton classes))
(define expr         (apply proc (map parameter args)))
(define result-type  (type expr))
(define intermediate (parameter result-type))
(define types        (map class-of args))

(code intermediate expr)

;(tensor (inject + j (dim i (get (arr (1 2 3) (4 5 6)) j i))))

(test-end "playground")
