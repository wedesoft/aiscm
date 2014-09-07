(define-module (aiscm xorg)
  #:use-module (oop goops)
  #:export (make-xdisplay xdisplay-shape xdisplay-process-events xdisplay-event-loop xdisplay-close
            xdisplay-quit? xdisplay-quit=
            make-xwindow xwindow-show xwindow-write xwindow-hide xwindow-close))
(load-extension "libguile-xorg" "init_xorg")
(define (xdisplay-shape self) (list (xdisplay-width self) (xdisplay-height self)))
