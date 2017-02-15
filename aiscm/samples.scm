(define-module (aiscm samples)
  #:use-module (oop goops)
  #:use-module (aiscm util)
  #:use-module (aiscm element)
  #:export (<samples> <meta<samples>> count planar?)
  #:re-export (typecode channels))

(define-class* <samples> <object> <meta<samples>> <class>
              (typecode #:init-keyword #:typecode #:getter typecode)
              (count    #:init-keyword #:count    #:getter count   )
              (channels #:init-keyword #:channels #:getter channels)
              (offsets  #:init-keyword #:offsets  #:getter offsets )
              (planar   #:init-keyword #:planar   #:getter planar? ))
