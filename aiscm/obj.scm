(define-module (aiscm obj)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (aiscm util)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm asm)
  #:use-module (aiscm scalar)
  #:export (<obj> <meta<obj>>
            scm-sum scm-difference scm-product scm-divide scm-remainder
            scm-logand scm-logior scm-logxor))
(define-class* <obj> <scalar> <meta<obj>> <meta<scalar>>)
(define-method (size-of (self <meta<obj>>)) 8)
;TODO: size-of
;TODO: pack
;TODO: unpack
(define-method (coerce (a <meta<obj>>) b) <obj>)
(define-method (coerce a (b <meta<obj>>)) <obj>)
;TODO: coerce
;TODO: write
;TODO: native-type
(define-method (build (self <meta<obj>>) value) (make self #:value (pointer->scm (make-pointer value))))
(define-method (content (type <meta<obj>>) self) (list (pointer-address (scm->pointer self))))

(define main (dynamic-link))
(define scm-sum        (dynamic-func "scm_sum"        main))
(define scm-difference (dynamic-func "scm_difference" main))
(define scm-product    (dynamic-func "scm_product"    main))
(define scm-divide     (dynamic-func "scm_divide"     main))
(define scm-remainder  (dynamic-func "scm_remainder"  main))
(define scm-logand     (dynamic-func "scm_logand"     main))
(define scm-logior     (dynamic-func "scm_logior"     main))
(define scm-logxor     (dynamic-func "scm_logxor"     main))
