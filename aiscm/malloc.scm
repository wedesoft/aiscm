(define-module (aiscm malloc)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (make-malloc
            malloc-plus
            malloc-read
            malloc-write))
(load-extension "libguile-malloc" "init_malloc")
(define* (malloc-read malloc size #:optional (uvec_type 'vu8))
  (pointer->bytevector (car malloc) size 0 uvec_type))
(define* (malloc-write malloc bv)
  (begin
    (bytevector-copy!
      bv 0
      (pointer->bytevector (car malloc) (caddr malloc)) 0
      (bytevector-length bv))
    bv))
