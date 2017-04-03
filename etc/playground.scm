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

(define-syntax-rule (d expr) (format #t "~a = ~a~&" (quote expr) expr))

(test-begin "tensors")


;(define s (parameter (sequence <ubyte>)))
;(define t (parameter (sequence <ubyte>)))
(define m (parameter (multiarray <ubyte> 2)))
;(define f (+ s t))
;(define g (tensor (dimension s) k (+ (get s k) (get t k))))

;(define-method (lookups (self <function>) (idx <var>)) (append-map (cut lookups <> idx) (arguments self)))

(define-method (lookups (self <indexer>)) (lookups self (index self)))
(define-method (lookups (self <indexer>) (idx <var>)) (lookups (delegate self) idx))
(define-method (lookups (self <lookup>) (idx <var>)) (if (eq? (index self) idx) (list self) (lookups (delegate self) idx)))
(define-method (lookups (self <function>)) (append-map lookups (arguments self)))
(define-method (lookups (self <function>) (idx <var>)) (append-map (cut lookups <> idx) (arguments self)))

(let [(s (parameter (sequence <ubyte>)))
      (u (parameter (sequence <ubyte>)))
      (m (parameter (multiarray <ubyte> 2)))
      (i (var <long>))]
  (test-equal "get lookup object of sequence"
    (list (delegate s)) (lookups s))
  (test-equal "get first lookup object of 2D array"
    (list (delegate (delegate m))) (lookups m))
  (test-equal "get second lookup object of 2D array"
    (list (delegate (delegate (delegate m)))) (lookups (delegate m)))
  (test-equal "get lookup objects of binary plus"
    (list (delegate s) (delegate u)) (lookups (+ s u)))
  (test-equal "get lookup based on same object when using tensor"
    (list (delegate (delegate s))) (map delegate (lookups (tensor (dimension s) k (get s k)))))
  (test-equal "get lookup using replaced variable"
    (list i) (map index (lookups (indexer (dimension s) i (get s i)))))
  (test-equal "get lookup based on same objects when using binary tensor"
    (list (delegate (delegate s)) (delegate (delegate u)))
    (map delegate (lookups (indexer (dimension s) i (+ (get s i) (get u i))))))
  (test-equal "get lookup using replaced variable"
    (list i i) (map index (lookups (indexer (dimension s) i (+ (get s i) (get u i)))))))


;(lookups g)

; TODO: lookups f?

(test-end "tensors")
