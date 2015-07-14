(define-module (aiscm float)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (aiscm util)
  #:use-module (aiscm element)
  #:export (float single double precision double?
            <float<>> <meta<float<>>>
            <float>  <float<single>> <meta<float<single>>>
            <double> <float<double>> <meta<float<double>>>))
(define single 'single)
(define double 'double)
(define-class <meta<float<>>> (<meta<element>>))
(define-class <float<>> (<element>)
              #:metaclass <meta<float<>>>)
(define-method (write (self <float<>>) port)
  (format port "#<~a ~a>" (class-name (class-of self)) (get-value self)))
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
(define (double? self) (eq? double (precision self)))
(define-method (size-of (self <meta<float<single>>>)) 4)
(define-method (size-of (self <meta<float<double>>>)) 8)
(define-method (pack (self <float<>>))
  (let* [(typecode (class-of self))
         (retval   (make-bytevector (size-of typecode)))
         (setter   (if (double? typecode)
                       bytevector-ieee-double-native-set!
                       bytevector-ieee-single-native-set!))]
    (setter retval 0 (get-value self))
    retval))
(define-method (unpack (self <meta<float<>>>) (packed <bytevector>))
  (let* [(ref   (if (double? self) bytevector-ieee-double-native-ref bytevector-ieee-single-native-ref))
         (value (ref packed 0))]
    (make self #:value value)))
(define-method (types (self <meta<float<>>>)) (list self))
(define-method (content (self <real>)) (list self))
