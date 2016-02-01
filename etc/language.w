,language wisp
use-modules : oop goops
              ice-9 optargs

+ 2 3
append '(1) '(2 3)

define-method : + (a <list>) (b <list>)
  append a b

define-class <a> :
  x #:init-value 0 #:init-keyword #:x #:getter get-x
  y #:init-value 0 #:init-keyword #:y #:getter get-y

define a : make <a> #:x 123 #:y 456

define-method : + (u <a>) (v <a>)
  make <a>
  . #:x : + (get-x u) (get-x v)
  . #:y : + (get-y u) (get-y v)

get-x {a + a}
