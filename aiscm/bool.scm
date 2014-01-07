(define-module (aiscm bool)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:export (<bool>
            make-bool
            pack
            unpack-bool))
(define-class <element> ()
  (value #:init-keyword #:value))
(define-class <bool> (<element>))
(define (make-bool value)
  (make <bool> #:value value))
(define-method (pack (self <bool>))
  (u8-list->bytevector (list (if (slot-ref self 'value) 1 0))))
(define (unpack-bool packed)
  (make-bool (if (eq? (car (bytevector->u8-list packed)) 0) #f #t)))
(define-generic equal?)
(define-method (equal? (a <bool>) (b <bool>))
  (equal? (slot-ref a 'value) (slot-ref b 'value)))
