(define-module (aiscm float)
  #:use-module (oop goops)
  #:use-module (aiscm util)
  #:use-module (aiscm element)
  #:export (float single double precision
            <float<>> <meta<float<>>>
            <float>  <float<single>> <meta<float<single>>>
            <double> <float<double>> <meta<float<double>>>))
(define single 'single)
(define double 'double)
(define-class <meta<float<>>> (<meta<element>>))
(define-class <float<>> (<element>)
              #:metaclass <meta<float<>>>)
(define-generic precision)
(define (float prec)
  (let* [(name      (format #f "<float<~a>>" prec))
         (metaname  (format #f "<meta~a>" name))
         (metaclass (def-once metaname (make <class>
                                             #:dsupers (list <meta<float<>>>)
                                             #:name metaname)))
         (retval    (def-once name (make metaclass
                                         #:dsupers (list <float<>>)
                                         #:name name)))]
    (define-method (precision (self metaclass)) prec)
    retval))
(define <float>  (float single))
(define <double> (float double))
(define-method (size-of (self <meta<float<single>>>)) 4)
(define-method (size-of (self <meta<float<double>>>)) 8)
