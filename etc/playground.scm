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

(define-syntax test
  (lambda (x)
    (syntax-case x ()
      ((k expr)           #'(list expr))
      ((k index rest ... expr) #'(cons (quote index) (test rest ... expr))))))

(test i 0)
(test i j 0)
(test i j k 0)

(define-syntax-rule (dim index expr) (let [(index (var <long>))] (indexer index expr (dimension-hint index))))

(define-syntax dim
  (lambda (x)
    (syntax-case x ()
      ((dim expr) #'expr)
      ((dim index indices ... expr) #'(let [(index (var <long>))] (indexer index (dim indices ... expr) (dimension-hint index)))))))

(tensor (dim (arr (2 3 5))))

(tensor (dim i (dim j (get (arr (2 3 5)) i j))))

(tensor (dim j i (get (arr (2 3 5)) i j)))

(test-end "playground")
