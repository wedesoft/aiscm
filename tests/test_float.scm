(use-modules (oop goops)
             (aiscm element)
             (aiscm float)
             (guile-tap))
(planned-tests 13)
(ok (equal? (float single) (float single))
    "equality of classes")
(ok (eq? single (precision (float single)))
    "determine precision of single-precision floating point class")
(ok (eq? double (precision (float double)))
    "determine precision of double-precision floating point class")
(ok (not (double? (float single)))
    "check whether single-precision floating point is double")
(ok (double? (float double))
    "check whether double-precision floating point is double")
(ok (equal? <float> (float single))
    "equality of predefined clases")
(ok (eqv? 4 (size-of <float>))
    "size of single-precision floating point number")
(ok (eqv? 8 (size-of <double>))
    "size of double-precision floating point number")
(ok (equal? #vu8(#x00 #x00 #xc0 #x3f) (pack (make <float> #:value 1.5)))
    "pack single-precision floating point number")
(ok (equal? #vu8(#x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40)
            (pack (make <double> #:value 3.14)))
    "pack double-precision floating point number")
(ok (equal? "#<<float<single>> 3.14>"
            (call-with-output-string (lambda (port) (display (make <float> #:value 3.14) port))))
    "display floating point object")
(ok (equal? (make <float> #:value 1.5) (unpack <float> #vu8(#x00 #x00 #xc0 #x3f)))
    "unpack single-precision floating point number")
(ok (equal? (make <double> #:value 3.14) (unpack <double> #vu8(#x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40)))
    "unpack double-precision floating point number")
; TODO: coerce
; TODO: equal
; TODO: match
; TODO: set
; TODO: get
; TODO: types
; TODO: content
; TODO: param
