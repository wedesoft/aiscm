(define-module (aiscm samples)
  #:use-module (oop goops)
  #:use-module (aiscm util)
  #:use-module (aiscm element)
  #:export (<samples> <meta<samples>> count planar?)
  #:re-export (typecode shape channels))

(define-class* <samples> <object> <meta<samples>> <class>
              (typecode #:init-keyword #:typecode #:getter typecode)
              (shape    #:init-keyword #:shape    #:getter shape   )
              (offsets  #:init-keyword #:offsets  #:getter offsets )
              (planar   #:init-keyword #:planar   #:getter planar? )
              (mem      #:init-keyword #:mem                       ))

(define-method (channels (self <samples>))
  "Get number of audio channels of audio samples"
  (car (shape self)))
