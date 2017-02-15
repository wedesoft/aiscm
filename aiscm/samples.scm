(define-module (aiscm samples)
  #:use-module (oop goops)
  #:use-module (aiscm util)
  #:export (<samples> <meta<samples>>))

(define-class* <samples> <object> <meta<samples>> <class>
              (format   #:init-keyword #:format   #:getter get-format  )
              (count    #:init-keyword #:count    #:getter get-count   )
              (channels #:init-keyword #:channels #:getter get-channels)
              (planar   #:init-keyword #:planar   #:getter get-planar  ))
