(define-module (aiscm obj)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (aiscm util)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm asm)
  #:use-module (aiscm scalar)
  #:export (<obj> <meta<obj>>
            obj-negate scm-lognot obj-zero-p obj-nonzero-p obj-not scm-sum
            scm-difference scm-product scm-divide scm-remainder
            scm-logand scm-logior scm-logxor scm-min scm-max scm-ash obj-shr
            obj-equal-p obj-nequal-p obj-less-p obj-leq-p obj-gr-p obj-geq-p))
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
(define guile-aiscm-obj (dynamic-link "libguile-aiscm-obj"))

(define obj-negate     (dynamic-func "obj_negate"     guile-aiscm-obj))
(define scm-lognot     (dynamic-func "scm_lognot"     main           ))
(define obj-zero-p     (dynamic-func "obj_zero_p"     guile-aiscm-obj))
(define obj-nonzero-p  (dynamic-func "obj_nonzero_p"  guile-aiscm-obj))
(define obj-not        (dynamic-func "obj_not"        guile-aiscm-obj))
(define scm-sum        (dynamic-func "scm_sum"        main           ))
(define scm-difference (dynamic-func "scm_difference" main           ))
(define scm-product    (dynamic-func "scm_product"    main           ))
(define scm-divide     (dynamic-func "scm_divide"     main           ))
(define scm-remainder  (dynamic-func "scm_remainder"  main           ))
(define scm-logand     (dynamic-func "scm_logand"     main           ))
(define scm-logior     (dynamic-func "scm_logior"     main           ))
(define scm-logxor     (dynamic-func "scm_logxor"     main           ))
(define scm-min        (dynamic-func "scm_min"        main           ))
(define scm-max        (dynamic-func "scm_max"        main           ))
(define scm-ash        (dynamic-func "scm_ash"        main           ))
(define obj-shr        (dynamic-func "obj_shr"        guile-aiscm-obj))
(define obj-equal-p    (dynamic-func "obj_equal_p"    guile-aiscm-obj))
(define obj-nequal-p   (dynamic-func "obj_nequal_p"   guile-aiscm-obj))
(define obj-less-p     (dynamic-func "obj_less_p"     guile-aiscm-obj))
(define obj-leq-p      (dynamic-func "obj_leq_p"      guile-aiscm-obj))
(define obj-gr-p       (dynamic-func "obj_gr_p"       guile-aiscm-obj))
(define obj-geq-p      (dynamic-func "obj_geq_p"      guile-aiscm-obj))
