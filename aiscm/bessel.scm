(define-module (math bessel)
  #:export (j0
            malloc))
;(use-modules (rnrs bytevectors))
(load-extension "guile-bessel" "init_bessel")
;(define (malloc size)
;  (make-bytevector size))
