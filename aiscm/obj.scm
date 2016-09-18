(define-module (aiscm obj)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (aiscm util)
  #:use-module (aiscm element)
  #:use-module (aiscm scalar)
  #:export (<obj> <meta<obj>>)) 
(define-class* <obj> <scalar> <meta<obj>> <meta<scalar>>)
;TODO: size-of
;TODO: pack
;TODO: unpack
;TODO: coerce
;TODO: write
;TODO: native-type
;TODO: build
(define-method (content self) (format #t "~a~&" self) (list (pointer-address (scm->pointer self))))
