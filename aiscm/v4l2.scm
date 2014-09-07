(define-module (aiscm v4l2)
  #:use-module (oop goops)
  #:use-module (aiscm mem)
  #:use-module (aiscm int)
  #:use-module (aiscm frame)
  #:use-module (aiscm sequence)
  #:use-module (system foreign)
  #:export (make-v4l2 v4l2-close v4l2-read))
(load-extension "libguile-v4l2" "init_v4l2")
(define (v4l2-read self)
  (let [(picture (v4l2-read-orig self))]
    (make <frame>
          #:format PIX_FMT_YUYV422; TODO: determine correct format
          #:width  (cadr picture)
          #:height (caddr picture)
          #:data   (cadddr picture))))
