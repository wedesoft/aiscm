(define-module (aiscm v4l2)
  #:use-module (system foreign)
  #:export (make-v4l2 close-v4l2))
(load-extension "libguile-v4l2" "init_v4l2")
