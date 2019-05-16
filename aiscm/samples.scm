(define-module (aiscm samples)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (aiscm util)
  #:use-module (aiscm core)
  #:export (<samples> <meta<samples>>
            AV_SAMPLE_FMT_U8 AV_SAMPLE_FMT_S16 AV_SAMPLE_FMT_S32 AV_SAMPLE_FMT_FLT AV_SAMPLE_FMT_DBL
            AV_SAMPLE_FMT_U8P AV_SAMPLE_FMT_S16P AV_SAMPLE_FMT_S32P AV_SAMPLE_FMT_FLTP AV_SAMPLE_FMT_DBLP
            planar? to-samples convert-samples convert-samples-from! type+planar->sample-format
            sample-format->type sample-format->planar from-samples)
  #:re-export (typecode shape size-of))

(load-extension "libguile-aiscm-samples" "init_samples")

(define-class* <samples> <object> <meta<samples>> <class>
              (typecode    #:init-keyword #:typecode    #:getter typecode   )
              (shape       #:init-keyword #:shape       #:getter shape      )
              (rate        #:init-keyword #:rate        #:getter rate       )
              (offsets     #:init-keyword #:offsets     #:getter offsets    )
              (planar      #:init-keyword #:planar      #:getter planar?    )
              (memory      #:init-keyword #:memory      #:getter memory     )
              (memory-base #:init-keyword #:memory-base #:getter memory-base))

(define-method (initialize (self <samples>) initargs)
  "Convert for images"
  (let-keywords initargs #f (typecode shape rate offsets planar memory memory-base)
    (let* [(offsets     (or offsets (if planar (iota (cadr shape) 0 (* (size-of typecode) (car shape))) '(0))))
           (memory      (or memory (gc-malloc-pointerless (apply * (size-of typecode) shape))))
           (memory-base (or memory-base memory))]
      (next-method self (list #:typecode    typecode
                              #:shape       shape
                              #:rate        rate
                              #:offsets     offsets
                              #:planar      planar
                              #:memory      memory
                              #:memory-base memory-base)))))

(define-method (channels (self <samples>))
  "Get number of audio channels of audio samples"
  (cadr (shape self)))

(define-method (size-of (self <samples>))
  "Memory size of audio samples in bytes"
  (apply * (size-of (typecode self)) (shape self)))

(define (from-samples self)
  "Convert audio samples to a numerical array"
  (if (planar? self)
      (from-samples (convert-samples self (typecode self) #f))
      (make (multiarray (typecode self) 2) #:shape (shape self) #:memory (memory self) #:memory-base (memory-base self))))

(define (to-samples self rate)
  "Convert numerical array to audio samples"
  (let [(shape     (if (eqv? (dimensions self) 1) (attach (shape self) 1) (shape self)))
        (compacted (ensure-default-strides self))]
    (make <samples> #:typecode    (typecode self)
                    #:shape       shape
                    #:planar      #f
                    #:rate        rate
                    #:memory      (memory compacted)
                    #:memory-base (memory-base compacted))))

(define typemap-packed
  (list (cons <ubyte>  AV_SAMPLE_FMT_U8  )
        (cons <sint>   AV_SAMPLE_FMT_S16 )
        (cons <int>    AV_SAMPLE_FMT_S32 )
        (cons <float>  AV_SAMPLE_FMT_FLT )
        (cons <double> AV_SAMPLE_FMT_DBL )))

(define typemap-planar
  (list (cons <ubyte>  AV_SAMPLE_FMT_U8P )
        (cons <sint>   AV_SAMPLE_FMT_S16P)
        (cons <int>    AV_SAMPLE_FMT_S32P)
        (cons <float>  AV_SAMPLE_FMT_FLTP)
        (cons <double> AV_SAMPLE_FMT_DBLP)))

(define inverse-typemap
  (append (alist-invert typemap-packed) (alist-invert typemap-planar)))

(define (type+planar->sample-format type planar)
  "Convert type and planar/packed information to type tag"
  (or (assq-ref (if planar typemap-planar typemap-packed) type)
      (aiscm-error 'type+planar->sample-format "Type ~a not supported by FFmpeg audio" type)))

(define (sample-format->type sample-format)
  "Get type information for type tag"
  (assq-ref inverse-typemap sample-format))

(define (sample-format->planar sample-format)
  "Check whether a sample format is planar"
  (not (memv sample-format (map cdr typemap-packed))))

(define (descriptor self)
  (list (type+planar->sample-format (typecode self) (planar? self)) (shape self) (rate self) (offsets self)))

(define (convert-samples-from! destination source)
  "Convert audio samples from source to destination format"
  (let [(source-type (descriptor source))
        (dest-type   (descriptor destination))]
    (samples-convert (memory source) source-type (memory destination) dest-type)))

(define (convert-samples self typecode planar)
  "Convert audio samples using the specified attributes"
  (let* [(size        (apply * (size-of typecode) (shape self)))
         (destination (make <samples>
                            #:typecode typecode
                            #:shape    (shape self)
                            #:rate     (rate self)
                            #:planar   planar
                            #:memory   (gc-malloc-pointerless size)))]
    (convert-samples-from! destination self)
    destination))
