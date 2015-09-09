(define-module (aiscm rgb)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 optargs)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm util)
  #:export (rgb type
            <rgb> reg green blue
            <rgb<>> <meta<rgb<>>>
            <ubytergb> <rgb<int<8,unsigned>>>  <meta<rgb<int<8,unsigned>>>>
            <bytergb>  <rgb<int<8,signed>>>    <meta<rgb<int<8,signed>>>>
            <usintrgb> <rgb<int<16,unsigned>>> <meta<rgb<int<16,unsigned>>>>
            <sintrgb>  <rgb<int<16,signed>>>   <meta<rgb<int<16,signed>>>>
            <uintrgb>  <rgb<int<32,unsigned>>> <meta<rgb<int<32,unsigned>>>>
            <intrgb>   <rgb<int<32,signed>>>   <meta<rgb<int<32,signed>>>>
            <ulonggb>  <rgb<int<64,unsigned>>> <meta<rgb<int<64,unsigned>>>>
            <longrgb>  <rgb<int<64,signed>>>   <meta<rgb<int<64,signed>>>>))
(define-class <rgb> ()
  (red   #:init-keyword #:red   #:getter red)
  (green #:init-keyword #:green #:getter green)
  (blue  #:init-keyword #:blue  #:getter blue))
(define-method (rgb (r <real>) (g <real>) (b <real>)) (make <rgb> #:red r #:green g #:blue b))
(define-method (write (self <rgb>) port)
  (format port "(rgb ~a ~a ~a)" (red self) (green self) (blue self)))
(define-method (equal? (a <rgb>) (b <rgb>))
  (and (equal? (red a) (red b)) (equal? (green a) (green b)) (equal? (blue a) (blue b))))
(define-class* <rgb<>> <element> <meta<rgb<>>> <meta<element>>)
(define-method (rgb (t <meta<element>>))
  (template-class (rgb t) <rgb<>>
    (lambda (class metaclass)
      (define-method (type (self metaclass))t); TODO: rename this
      (define-method (size-of (self metaclass)) (* 3 (size-of t))))))
(define-method (write (self <rgb<>>) port)
  (format port "#<~a ~a>" (class-name (class-of self)) (get-value self)))
(define-method (pack (self <rgb<>>))
  (let* [(vals     (list (red (get-value self)) (green (get-value self)) (blue (get-value self))))
         (channels (map (cut make (type (class-of self)) #:value <>) vals))
         (size     (size-of (type (class-of self))))]
    (bytevector-concat (map pack channels))))
(define-method (unpack (self <meta<rgb<>>>) (packed <bytevector>))
  (let* [(size    (size-of (type self)))
         (vectors (map (cut bytevector-sub packed <> size) (map (cut * size <>) (iota 3))))]
    (make self #:value (apply rgb (map (lambda (vec) (get (unpack (type self) vec))) vectors)))))
(define <ubytergb> (rgb <ubyte>))
(define <bytergb>  (rgb <byte>))
(define <usintrgb> (rgb <usint>))
(define <sintrgb>  (rgb <sint>))
(define <uintrgb>  (rgb <uint>))
(define <intrgb>   (rgb <int>))
(define <ulongrgb> (rgb <ulong>))
(define <longrgb>  (rgb <long>))
(define-method (coerce (a <meta<rgb<>>>) (b <meta<element>>)) (rgb (coerce (type a) b)))
(define-method (coerce (a <meta<element>>) (b <meta<rgb<>>>)) (rgb (coerce a (type b))))
