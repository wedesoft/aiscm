(define-module (aiscm xorg)
  #:use-module (oop goops)
  #:export (make-xdisplay xdisplay-close xdisplay-shape
            make-xwindow xwindow-show xwindow-hide xwindow-close))
(load-extension "libguile-xorg" "init_xorg")
(define (xdisplay-shape self) (list (xdisplay-width self) (xdisplay-height self)))
