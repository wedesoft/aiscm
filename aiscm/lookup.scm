(define-module (aiscm lookup)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm pointer)
  #:use-module (aiscm var)
  #:export (<lookup>
            make-lookup
            get-index
            get-stride))
(define-class <lookup> (<element>)
  (index #:init-keyword #:index #:getter get-index)
  (stride #:init-keyword #:stride #:getter get-stride))
(define (make-lookup value index stride)
  (make <lookup> #:value value #:index index #:stride stride))
(define-method (equal? (a <lookup>) (b <lookup>))
  (and
    (next-method)
    (equal? (get-index a) (get-index b))
    (equal? (get-stride a) (get-stride b))))
(define-method (lookup (self <pointer<>>) (value <var>) (stride <integer>))
  (make-lookup self value stride))
(define-method (subst (self <lookup>) alist)
  (lookup (get-value self) (subst (get-index self) alist) (get-stride self)))
