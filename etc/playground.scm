(use-modules (oop goops)
             (srfi srfi-1)
             (ice-9 curried-definitions)
             (aiscm operation)
             (aiscm variable)
             (aiscm expression)
             (aiscm asm)
             (aiscm element)
             (aiscm scalar)
             (aiscm int)
             (aiscm sequence)
             (aiscm rgb)
             (aiscm jit)
             (aiscm tensor)
             (aiscm util))

(test-begin "playground")

(tensor i (get (sum j (get (arr (1 1 1) (1 1 1)) j)) i))

(define m (parameter (multiarray <int> 2)))

;(define e (dim i (get (sum j (get m j)) i)))

(define s (sum j (get m j)))

(define i (var <long>))

(define e (get s i))

(test-end "playground")
