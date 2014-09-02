(define-module (aiscm xorg)
  #:use-module (oop goops)
  #:export (make-xdisplay close-xdisplay shape-display
            make-xwindow show-xwindow hide-xwindow close-xwindow))
(load-extension "libguile-xorg" "init_xorg")
(define (shape-display self) (list (width-display self) (height-display self)))
