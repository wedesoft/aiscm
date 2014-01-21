(define-module (aiscm element)
  #:use-module (oop goops)
  #:export (<element>
            get-value
            storage-size
            pack
            unpack
            subst
            typecode
            size
            shape
            dimension
            lookup
            skip))
(define-class <element> ()
  (value #:init-keyword #:value #:getter get-value))
(define-generic storage-size)
(define-generic pack)
(define-generic unpack)
(define-method (equal? (a <element>) (b <element>))
  (equal? (get-value a) (get-value b)))
(define-method (subst (self <element>) alist) self)
(define-method (size (self <element>)) 1)
(define-method (shape (self <element>)) '())
(define-generic typecode)
(define-generic dimension)
(define-generic lookup)
(define-generic skip)
