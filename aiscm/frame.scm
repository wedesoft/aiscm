(define-module (aiscm frame)
  #:use-module (oop goops)
  #:export (PIX_FMT_GRAY8 PIX_FMT_YUYV422 frame-convert))
(load-extension "libguile-frame" "init_frame")
