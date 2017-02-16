(define-module (aiscm samples)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (aiscm mem)
  #:use-module (aiscm util)
  #:use-module (aiscm element)
  #:use-module (aiscm sequence)
  #:export (<samples> <meta<samples>> count planar? to-samples convert-samples)
  #:re-export (typecode shape channels rate to-array))

;(load-extension "libguile-aiscm-samples" "init_samples")

(define-class* <samples> <object> <meta<samples>> <class>
              (typecode #:init-keyword #:typecode #:getter typecode)
              (shape    #:init-keyword #:shape    #:getter shape   )
              (rate     #:init-keyword #:rate     #:getter rate    )
              (offsets  #:init-keyword #:offsets  #:getter offsets )
              (planar   #:init-keyword #:planar   #:getter planar? )
              (mem      #:init-keyword #:mem                       ))

(define-method (initialize (self <samples>) initargs)
  "Convert for images"
  (let-keywords initargs #f (typecode shape rate planar mem)
    (let [(offsets '(0))]
      (next-method self (list #:typecode typecode
                              #:shape    shape
                              #:rate     rate
                              #:offsets  offsets
                              #:planar   planar
                              #:mem      mem)))))

(define-method (channels (self <samples>))
  "Get number of audio channels of audio samples"
  (car (shape self)))

(define-method (to-array (self <samples>))
  "Convert audio samples to a numerical array"
  (make (multiarray (typecode self) 2) #:shape (shape self) #:value (slot-ref self 'mem)))

(define-method (to-samples (self <sequence<>>))
  "Convert numerical array to audio samples"
  (make <samples> #:typecode (typecode self) #:shape (shape self) #:planar #f #:mem (slot-ref self 'value)))

(define (descriptor self)
  (list (typecode self) (shape self) (rate self) (offsets self) (planar? self)))

(define (convert-samples self typecode rate planar)
  "Convert audio samples using the specified attributes"
  (let* [(destination (make <samples>
                            #:typecode typecode
                            #:shape (shape self)
                            #:planar planar
                            #:mem (make <mem> #:size 1000 #:pointerless #t))); TODO: compute size
         (dest-type   (descriptor destination))
         (source-type (descriptor self))]
    (samples-convert (get-memory (slot-ref self 'mem)) source-type (get-memory (slot-ref  destination 'mem)) dest-type)
    self))
