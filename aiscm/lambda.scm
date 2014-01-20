(define-module (aiscm lambda)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm pointer)
  #:export (<lambda>
            make-lambda
            get-index
            ref))
(define-class <lambda> (<element>)
  (index #:init-keyword #:index #:getter get-index))
(define (make-lambda index term)
  (make <lambda> #:index index #:value term))
(define-method (ref (self <lambda>) (i <integer>))
  (fetch (subst (get-value self) (list (cons (get-index self) i)))))
(define-method (typecode (self <lambda>))
  (typecode (get-value self)))
(define-method (size (self <lambda>))
  (let ((value (get-value self)))
    (* (dimension value (get-index self)) (size value))))
