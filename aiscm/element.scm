(define-module (aiscm element)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:export (<element>
            <meta<element>>
            get-value
            storage-size
            foreign-type
            pack
            unpack
            subst
            typecode
            size
            shape
            lookup
            shift
            slice
            coerce))
(define-class <meta<element>> (<class>))
(define-class <element> ()
  (value #:init-keyword #:value #:getter get-value))
(define-generic storage-size)
(define-generic foreign-type)
(define-generic pack)
(define-generic unpack)
(define-method (equal? (a <element>) (b <element>))
  (equal? (get-value a) (get-value b)))
(define-method (subst (self <element>) alist) self)
(define-method (size (self <element>)) 1)
(define-method (shape (self <element>)) '())
(define-generic typecode)
(define-generic lookup)
(define-generic shift)
(define-generic slice)
(define-generic coerce)
