(define-module (aiscm malloc)
  #:export (make-malloc
            malloc-plus))
(load-extension "libguile-malloc" "init_malloc")
