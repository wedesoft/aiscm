(define-module (aiscm method)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:export (<native-method> <native-value>
            native-method function-pointer return-type argument-types native-value
            scm-gc-malloc-pointerless scm-gc-malloc)
  #:re-export (get))

(define-class <native-method> ()
  (function-pointer #:init-keyword #:function-pointer #:getter function-pointer)
  (return-type      #:init-keyword #:return-type      #:getter return-type)
  (argument-types   #:init-keyword #:argument-types   #:getter argument-types))

(define (native-method return-type argument-types function-pointer)
  (make <native-method> #:function-pointer function-pointer
                        #:return-type      return-type
                        #:argument-types   argument-types))

(define-class <native-value> ()
  (value       #:init-keyword #:value       #:getter get)
  (return-type #:init-keyword #:return-type #:getter return-type))

(define (native-value return-type value)
  (make <native-value> #:value value #:return-type return-type))

(define main (dynamic-link))
(define scm-gc-malloc-pointerless (native-method <ulong> (list <ulong>) (dynamic-func "scm_gc_malloc_pointerless" main)))
(define scm-gc-malloc             (native-method <ulong> (list <ulong>) (dynamic-func "scm_gc_malloc"             main)))
