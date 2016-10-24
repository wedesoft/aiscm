(define-module (aiscm method)
  #:use-module (oop goops)
  #:export (<native-method> native-method function-pointer return-type argument-types))

(define-class <native-method> ()
  (function-pointer #:init-keyword #:function-pointer #:getter function-pointer)
  (return-type      #:init-keyword #:return-type      #:getter return-type)
  (argument-types   #:init-keyword #:argument-types   #:getter argument-types))

(define (native-method return-type argument-types function-pointer)
  (make <native-method> #:function-pointer function-pointer
                        #:return-type      return-type
                        #:argument-types   argument-types))
