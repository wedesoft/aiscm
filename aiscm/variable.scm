(define-module (aiscm variable)
  #:use-module (oop goops)
  #:export (<variable>
            make-var
            subst))
(define-class <variable> ())
(define (make-var)
  (make <variable>))
(define-method (subst (self <variable>) (alist <pair>))
  (if (assoc self alist)
    (assoc-ref alist self)
    self))
