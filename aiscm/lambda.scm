(define-module (aiscm lambda)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:export (<lambda>
            make-lambda
            get-index
            ref))
(define-class <lambda> (<element>)
  (index #:init-keyword #:index #:getter get-index))
(define (make-lambda index term)
  (make <lambda> #:index index #:value term))
(define-method (ref (self <lambda>) (i <integer>))
  (subst (get-value self) (list (cons (get-index self) i))))
