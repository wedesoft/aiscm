(define-module (aiscm xorg)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (aiscm frame)
  #:export (<xdisplay> <meta<xdisplay>>
            <xwindow> <meta<xwindow>>
            shape process-events event-loop close quit? quit=
            show hide title= resize write
            IO-XIMAGE IO-OPENGL))
(load-extension "libguile-xorg" "init_xorg")
(define-class <meta<xdisplay>> (<class>))
(define-class <xdisplay> ()
              (display #:init-keyword #:display #:getter get-display)
              #:metaclass <meta<xdisplay>>)
(define-method (initialize (self <xdisplay>) initargs)
  (let-keywords initargs #f (name)
    (let [(name (or name ":0.0"))]
      (next-method self (list #:display (make-display name))))))
(define-method (shape (self <xdisplay>))
  (list (display-width (get-display self)) (display-height (get-display self))))
(define-method (process-events (self <xdisplay>)) (display-process-events (get-display self)))
(define-method (event-loop (self <xdisplay>) (timeout <real>))
  (display-event-loop (get-display self) timeout))
(define-method (event-loop (self <xdisplay>)) (display-event-loop (get-display self) -1))
; TODO: rename close
(define-method (close (self <xdisplay>)) (display-close (get-display self)))
(define-method (quit? (self <xdisplay>)) (display-quit? (get-display self)))
(define-method (quit= (self <xdisplay>) (value <boolean>)) (display-quit= (get-display self) value))
(define-class <meta<xwindow>> (<class>))
(define-class <xwindow> ()
              (window #:init-keyword #:window #:getter get-window)
              #:metaclass <meta<xwindow>>)
(define-method (initialize (self <xwindow>) initargs)
  (let-keywords initargs #f (display width height io)
    (let [(io (or io IO-XIMAGE))]
      (next-method self (list #:window (make-window (get-display display) width height io))))))
(define-method (show (self <xwindow>)) (window-show (get-window self)))
(define-method (hide (self <xwindow>)) (window-hide (get-window self)))
; TODO: rename close
(define-method (close (self <xwindow>)) (window-close (get-window self)))
(define-method (title= (self <xwindow>) (title <string>)) (window-title= (get-window self) title))
(define-method (resize (self <xwindow>) (width <integer>) (height <integer>))
  (window-resize (get-window self) width height))
; TOOD: rename write
(define-method (write (self <xwindow>) (frame <frame>))
  (window-write (get-window self) frame))
