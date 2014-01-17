(define-module (aiscm element)
  #:use-module (oop goops)
  #:export (<element>
            get-value
            storage-size
            pack
            unpack
            subst))
(define-class <element> ()
  (value #:init-keyword #:value #:getter get-value))
(define-generic storage-size)
(define-generic pack)
(define-generic unpack)
(define-method (equal? (a <element>) (b <element>))
  (equal? (get-value a) (get-value b)))
(define-method (subst (self <element>) alist)
  self)
