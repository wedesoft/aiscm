(define-module (aiscm frame)
  #:use-module (oop goops)
  #:export (<frame> <meta<frame>>
            get-format get-width get-height get-data convert
            PIX_FMT_YUYV422 PIX_FMT_GRAY8 PIX_FMT_BGRA))
(load-extension "libguile-frame" "init_frame")
(define-class <meta<frame>> (<class>))
(define-class <frame> ()
              (format #:init-keyword #:format #:getter get-format)
              (width #:init-keyword #:width #:getter get-width)
              (height #:init-keyword #:height #:getter get-height)
              (data #:init-keyword #:data #:getter get-data)
              #:metaclass <meta<frame>>)
(define-method (convert (self <frame>) (target <integer>))
  (let* [(width (get-width self))
         (height (get-height self))
         (result-data (frame-convert (get-format self) width height (get-data self) target))]
    (make <frame>
          #:format target
          #:width width
          #:height height
          #:data result-data)))
