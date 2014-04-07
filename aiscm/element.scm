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
            coerce
            match
            get
            set
            get-size))
(define-class <meta<element>> (<class>))
(define-class <element> ()
              (value #:init-keyword #:value #:getter get-value)
              #:metaclass <meta<element>>)
(define-generic storage-size)
(define-generic foreign-type)
(define-generic pack)
(define-generic unpack)
(define-method (equal? (a <element>) (b <element>))
  (equal? (get-value a) (get-value b)))
(define-method (subst (self <element>) alist) self)
(define-method (size (self <element>)) 1)
(define-method (shape (self <element>)) '())
(define-method (typecode (self <element>)) (typecode (class-of self)))
(define-generic lookup)
(define-generic shift)
(define-generic slice)
(define-generic coerce)
(define-generic match)
(define-method (get (self <element>)) (get-value self))
(define-method (set (self <element>) o) (begin (slot-set! self 'value o)) o)
(define-generic get-size)
