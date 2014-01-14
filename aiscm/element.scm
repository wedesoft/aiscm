(define-module (aiscm element)
  #:use-module (oop goops)
  #:export (<element>
            get-value
            storage-size
            pack
            unpack))
(define-class <element> ()
  (value #:init-keyword #:value #:getter get-value))
(define-generic storage-size)
(define-generic pack)
(define-generic unpack)
