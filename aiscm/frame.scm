(define-module (aiscm frame)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (aiscm util)
  #:export (<frame> <meta<frame>>
            get-format get-width get-height get-data convert
            PIX_FMT_YUYV422 PIX_FMT_GRAY8 PIX_FMT_BGRA))
(load-extension "libguile-frame" "init_frame")
(define-class <meta<frame>> (<class>))
(define-class <frame> ()
              (format #:init-keyword #:format #:getter get-format)
              (width #:init-keyword #:width #:getter get-width)
              (height #:init-keyword #:height #:getter get-height)
              (offsets #:init-keyword #:offsets #:getter get-offsets)
              (pitches #:init-keyword #:pitches #:getter get-pitches)
              (data #:init-keyword #:data #:getter get-data)
              #:metaclass <meta<frame>>)
(define-method (initialize (self <frame>) initargs)
  (let-keywords initargs #f (format width height offsets pitches data)
    (next-method self (list #:format format
                            #:width width
                            #:height height
                            #:offsets (or offsets (default-offsets format width height))
                            #:pitches (or pitches (default-pitches format width))
                            #:data data))))
(define formats
  (list (cons 'RGB  PIX_FMT_RGB24)
        (cons 'BGR  PIX_FMT_BGR24)
        (cons 'BGRA PIX_FMT_BGRA)
        (cons 'GRAY PIX_FMT_GRAY8)
        (cons 'I420 PIX_FMT_YUV420P)
        (cons 'UYVY PIX_FMT_UYVY422)
        (cons 'YUY2 PIX_FMT_YUYV422)))
(define symbols (assoc-invert formats))
(define (sym->fmt sym) (assq-ref formats sym))
(define (fmt->sym fmt) (assq-ref symbols fmt))
(define (frame-size format width height)
  (case format
    ((RGB)  (* width height 3))
    ((BGR)  (* width height 3))
    ((BGRA) (* width height 4))
    ((GRAY) (* width height))
    ((I420) (+ (* width height) (* (ash (+ width 1) -1) (ash (+ height 1) -1))))
    ((UYVY) (* (logand (+ width 3) (lognot #x3)) height 2))
    ((YUY2) (* (logand (+ width 3) (lognot #x3)) height 2))))
(define (default-offsets format width height)
  (case format
    ((RGB)  (list 0))
    ((BGR)  (list 0))
    ((BGRA) (list 0))
    ((GRAY) (list 0))
    ((I420) (list 0 (* width height) (+ (* width height) (* (ash (+ width 1) -1) (ash (+ width 1) -1)))))
    ((UYVY) (list 0))
    ((YUY2) (list 0))))
(define (default-pitches format width)
  (case format
    ((RGB)  (list (* width 3)))
    ((BGR)  (list (* width 3)))
    ((BGRA) (list (* width 4)))
    ((GRAY) (list width))
    ((I420) (list width (ash (+ width 1) -1) (ash (+ width 1) -1)))
    ((UYVY) (list (* 2 (logand (+ width 3) (lognot #x3)))))
    ((YUY2) (list (* 2 (logand (+ width 3) (lognot #x3)))))))
(define-method (descriptor (format <symbol>)
                           (width <integer>)
                           (height <integer>)
                           (offsets <list>)
                           (pitches <list>))
  (list (sym->fmt format) width height offsets pitches))
(define-method (descriptor (self <frame>))
  (descriptor (get-format self)
              (get-width self)
              (get-height self)
              (get-offsets self)
              (get-pitches self)))
(define-method (convert (self <frame>)
                        (format <symbol>)
                        (width <integer>)
                        (height <integer>)
                        (offsets <list>)
                        (pitches <list>))
  (let [(source-type (descriptor self))
        (dest-type   (descriptor format width height offsets pitches))]
    (if (equal? source-type dest-type)
      self
      (let [(data (bytevector->pointer (make-bytevector (frame-size format width height))))]
        (frame-convert (get-data self) source-type data dest-type)
        (make <frame> #:format format
                      #:width width
                      #:height height
                      #:data data
                      #:offsets offsets
                      #:pitches pitches)))))
(define-method (convert (self <frame>) (format <symbol>) (width <integer>) (height <integer>))
  (convert self
           format
           width
           height
           (default-offsets format width height)
           (default-pitches format width)))
(define-method (convert (self <frame>) (format <symbol>))
  (convert self format (get-width self) (get-height self)))
(define-method (write (self <frame>) port)
  (format port "#<<frame> ~a ~a ~a>" (get-format self) (get-width self) (get-height self)))
(define-method (display (self <frame>) port)
  (format port "#<<frame> ~a ~a ~a>" (get-format self) (get-width self) (get-height self)))
