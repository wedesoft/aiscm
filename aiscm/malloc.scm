(define-module (aiscm malloc)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (make-malloc
            malloc-plus
            malloc-read
            malloc-write))
(load-extension "libguile-malloc" "init_malloc")
(define (malloc-read malloc size)
  (if
    (> size (caddr malloc))
    (throw 'malloc-read-size-overrun size (caddr malloc))
    (pointer->bytevector (car malloc) size)))
(define (malloc-write malloc bv)
  (begin
    (bytevector-copy!
      bv 0
      (pointer->bytevector (car malloc) (caddr malloc)) 0
      (bytevector-length bv))
    bv))
