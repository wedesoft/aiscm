(define-module (aiscm var)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:export (<var>
            make-var
            get-type))
(define-class <var> (<element>))
(define (make-var)
  (make <var>))
(define-method (subst (self <var>) alist)
  (if (assq self alist)
    (assq-ref alist self)
    self))
