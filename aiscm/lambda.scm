(define-module (aiscm lambda)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm pointer)
  #:export (<lambda>
            get-index
            get-length))
(define-class <lambda> (<element>)
  (index #:init-keyword #:index #:getter get-index)
  (length #:init-keyword #:length #:getter get-length))
(define-method (get (self <lambda>) (i <integer>))
  (let [(ptr (subst (get-value self) (list (cons (get-index self) i))))]
    (get-value (fetch ptr))))
(define-method (set (self <lambda>) (i <integer>) value)
  (let [(ptr (subst (get-value self) (list (cons (get-index self) i))))
        (element (make (typecode self) #:value value))]
    (get-value (store ptr element))))
(define-method (typecode (self <lambda>))
  (typecode (get-value self)))
(define-method (size (self <lambda>))
  (* (get-length self) (size (get-value self))))
(define-method (shape (self <lambda>))
  (append (shape (get-value self)) (list (get-length self))))
(define-method (slice (self <lambda>) (offset <integer>) (length <integer>))
  (let [(value (shift (get-value self) (get-index self) offset))]
    (make <lambda> #:value value #:index (get-index self) #:length length)))
