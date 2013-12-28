(define-module (math aiscm)
  #:export (make-malloc
            malloc-plus))
(load-extension "libguile-aiscm" "init_aiscm")
