;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Wedekind <jan@wedesoft.de>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
(define-module (aiscm core)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 poe)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (aiscm util)
  #:export (get integer signed unsigned bits signed? coerce foreign-type
            floating-point single-precision double-precision double-precision?
            decompose-argument decompose-result decompose-type compose-value compose-values
            complex base size-of unpack-value native-type components constructor build
            pointer target llvmlist typecode dimension llvm-last llvm-all-but-last
            multiarray dimensions shape memory memory-base strides llvmarray
            llvm-void llvm-bool llvm-float llvm-double llvm-uint8 llvm-int8 llvm-uint16 llvm-int16
            llvm-uint32 llvm-int32 llvm-uint64 llvm-int64
            make-constant make-constant-pointer make-llvm-module make-function llvm-dump
            function-ret llvm-func get-type llvm-compile llvm-fetch llvm-store function-param
            make-basic-block position-builder-at-end build-branch build-cond-branch
            llvm-neg llvm-fneg llvm-not llvm-add llvm-fadd llvm-sub llvm-fsub llvm-mul llvm-fmul
            llvm-wrap llvm-trunc llvm-sext llvm-zext llvm-typed to-type return duplicate
            llvm-fp-cast llvm-fp-to-si llvm-fp-to-ui llvm-si-to-fp llvm-ui-to-fp
            llvm-call typed-call typed-constant typed-pointer store fetch llvm-begin to-list
            ~ le lt ge gt eq ne llvm-if typed-alloca to-array set rgb red green blue
            ensure-default-strides default-strides roll
            destroy read-image write-image read-audio write-audio rate channels
            <void> <meta<void>>
            <scalar> <meta<scalar>>
            <structure> <meta<structure>>
            <bool>  <meta<bool>>
            <int<>> <meta<int<>>>
            <ubyte> <meta<ubyte>> <int<8,unsigned>>  <meta<int<8,unsigned>>>
            <byte>  <meta<byte>>  <int<8,signed>>    <meta<int<8,signed>>>
            <usint> <meta<usint>> <int<16,unsigned>> <meta<int<16,unsigned>>>
            <sint>  <meta<sint>>  <int<16,signed>>   <meta<int<16,signed>>>
            <uint>  <meta<uint>>  <int<32,unsigned>> <meta<int<32,unsigned>>>
            <int>   <meta<int>>   <int<32,signed>>   <meta<int<32,signed>>>
            <ulong> <meta<ulong>> <int<64,unsigned>> <meta<int<64,unsigned>>>
            <long>  <meta<long>>  <int<64,signed>>   <meta<int<64,signed>>>
            <float<>> <meta<float<>>>
            <float>  <meta<float>>  <float<single>> <meta<float<single>>>
            <double> <meta<double>> <float<double>> <meta<float<double>>>
            <complex<>>       <meta<complex<>>>
            <complex<float>>  <meta<complex<float>>>  <complex<float<single>>> <meta<complex<float<single>>>>
            <complex<double>> <meta<complex<double>>> <complex<float<double>>> <meta<complex<float<double>>>>
            <pointer<>> <meta<pointer<>>>
            <multiarray<>> <meta<multiarray<>>> <llvmarray<>> <meta<llvmarray<>>>
            <llvm> <meta<llvm>>
            <llvm-function> <meta<llvm-function>>
            <rgb>
            <rgb<ubyte>> <meta<rgb<ubyte>>> <rgb<int<8,unsigned>>> <meta<rgb<int<8,unsigned>>>>
            <rgb<float>> <meta<rgb<float>>> <rgb<float<single>>> <meta<rgb<float<single>>>>
            <rgb<double>> <meta<rgb<double>>> <rgb<float<double>>> <meta<rgb<float<double>>>>)
  #:export-syntax (define-structure memoize define-uniform-constructor define-mixed-constructor llvm-set
                   llvm-while typed-let arr)
  #:re-export (- + * real-part imag-part))

(load-extension "libguile-aiscm-core" "init_core")

(define signed   'signed)
(define unsigned 'unsigned)

(define bool (1+ int64)); int64 is last foreign type

(define-class* <void> <object> <meta<void>> <class>
               (value #:init-keyword #:value #:getter get))

(define-class* <scalar> <void> <meta<scalar>> <meta<void>>)

(define-class* <structure> <void> <meta<structure>> <meta<void>>)

(define-class* <bool> <scalar> <meta<bool>> <meta<scalar>>)

(define-class* <int<>> <scalar> <meta<int<>>> <meta<scalar>>)

(define-class* <float<>> <scalar> <meta<float<>>> <meta<scalar>>)

(define-class* <pointer<>> <scalar> <meta<pointer<>>> <meta<scalar>>)

(define-class* <llvmlist<>> <structure> <meta<llvmlist<>>> <meta<structure>>)

(define-syntax-rule (component-accessor type name index)
  "Define accessor to access component of a composite type"
  (define-method (name (self type))
    (make (list-ref (base (class-of self)) index)
          #:value (lambda (fun) (list-ref ((get self) fun) index)))))

(define-syntax component-accessors
  (lambda (x)
    (syntax-case x ()
      ((k class metaclass members ...)
       "Define accessor methods for individual components of a composite type"
       (let [(n (length (syntax->datum #'(members ...))))]
         #`(begin
             (define-method (components (self metaclass))
               "List component accessor methods of composite type"
               (list members ...))
             .
             #,(map (lambda (member-name index)
                      #`(component-accessor class
                                            #,(datum->syntax #'k member-name)
                                            #,(datum->syntax #'k index)))
                    (syntax->datum #'(members ...))
                    (iota n))))))))

(define-syntax define-structure
  (lambda (x)
    (syntax-case x ()
      ((k name construct (members ...))
        (let [(class       (string->symbol (format #f "<~a<>>" (syntax->datum #'name))))
              (metaclass   (string->symbol (format #f "<meta<~a<>>>" (syntax->datum #'name))))
              (n           (length (syntax->datum #'(members ...))))
              (header      (map (cut list <> '<meta<void>>) (syntax->datum #'(members ...))))]
          #`(begin
              (define-class* #,(datum->syntax #'k class) <structure> #,(datum->syntax #'k metaclass) <meta<structure>>)

              (define-method (constructor (type #,(datum->syntax #'k metaclass)))
                "Get constructor for composite type"
                construct)

              (define-method (build (type #,(datum->syntax #'k metaclass)))
                "Get method for composing value in compiled code"
                name)

              (define-method (name #,@(datum->syntax #'k header))
                "Instantiate a composite type using the type template"
                (template-class (name members ...) #,(datum->syntax #'k class)
                  (lambda (class metaclass)
                    (define-method (base (self metaclass)) (list members ...)))))

              (define-method (name (base-type <meta<void>>))
                "Instantiate a composite type using the type template"
                (template-class (name base-type) #,(datum->syntax #'k class)
                  (lambda (class metaclass)
                    (define-method (base (self metaclass)) base (make-list #,(datum->syntax #'k n) base-type)))))

              (define-method (foreign-type (type #,(datum->syntax #'k metaclass)))
                "Foreign type of template class is pointer"
                int64)

              (component-accessors #,(datum->syntax #'k class) #,(datum->syntax #'k metaclass) members ...)))))))

(define (integer nbits sgn)
  "Retrieve integer class with specified number of bits and sign"
  (template-class (int nbits sgn) <int<>>
    (lambda (class metaclass)
      (define-method (bits (self metaclass)) nbits)
      (define-method (signed? (self metaclass)) (eq? sgn 'signed)))))

(define-method (bits (value <int<>>)) (bits (class-of value)))
(define-method (signed? (value <int<>>)) (signed? (class-of value)))

(define-method (signed? (cls <meta<pointer<>>>)) #f)

(define <ubyte> (integer  8 unsigned)) (define <meta<ubyte>> (class-of <ubyte>))
(define <byte>  (integer  8 signed  )) (define <meta<byte>>  (class-of <byte> ))
(define <usint> (integer 16 unsigned)) (define <meta<usint>> (class-of <usint>))
(define <sint>  (integer 16 signed  )) (define <meta<sint>>  (class-of <sint> ))
(define <uint>  (integer 32 unsigned)) (define <meta<uint>>  (class-of <uint> ))
(define <int>   (integer 32 signed  )) (define <meta<int>>   (class-of <int>  ))
(define <ulong> (integer 64 unsigned)) (define <meta<ulong>> (class-of <ulong>))
(define <long>  (integer 64 signed  )) (define <meta<long>>  (class-of <long> ))

(define single-precision 'single)
(define double-precision 'double)

(define (floating-point precision)
  (template-class (float precision) <float<>>
    (lambda (class metaclass)
      (define-method (double-precision? (self metaclass)) (eq? precision double-precision)) )))

(define <float>  (floating-point single-precision)) (define <meta<float>>  (class-of <float> ))
(define <double> (floating-point double-precision)) (define <meta<double>> (class-of <double>))

(define-structure complex make-rectangular (real-part imag-part))

(define <complex<float>>  (complex <float> )) (define <meta<complex<float>>>  (class-of (complex <float> )))
(define <complex<double>> (complex <double>)) (define <meta<complex<double>>> (class-of (complex <double>)))

(define-class <rgb> ()
  (red   #:init-keyword #:red   #:getter red)
  (green #:init-keyword #:green #:getter green)
  (blue  #:init-keyword #:blue  #:getter blue))

(define-method (rgb r g b)
  "Make RGB value"
  (make <rgb> #:red r #:green g #:blue b))

(define-method (write (self <rgb>) port)
  "Display RGB value"
  (format port "(rgb ~a ~a ~a)" (red self) (green self) (blue self)))

(define-method (equal? (a <rgb>) (b <rgb>))
  (and  (equal? (red a) (red b)) (equal? (green a) (green b)) (equal? (blue a) (blue b))))

(define-structure rgb rgb (red green blue))

(define <rgb<ubyte>>   (rgb <ubyte> )) (define <meta<rgb<ubyte>>>  (class-of (rgb <ubyte>)))
(define <rgb<float>>   (rgb <float> )) (define <meta<rgb<ubyte>>>  (class-of (rgb <float>)))
(define <rgb<double>>  (rgb <double>)) (define <meta<rgb<double>>> (class-of (rgb <double>)))

(define-method (target (self <pointer<>>)) (target (class-of self)))

(define (pointer tgt)
  "Create pointer class"
  (template-class (pointer tgt) <pointer<>>
    (lambda (class metaclass)
      (define-method (target (self metaclass)) tgt))))

(define-method (typecode (self <llvmlist<>>)) (typecode (class-of self)))

(define-method (dimension (self <llvmlist<>>)) (dimension (class-of self)))

(define-method (llvmlist (type <meta<void>>) (size <integer>))
  (template-class (llvmlist type size) <llvmlist<>>
    (lambda (class metaclass)
      (define-method (typecode (self metaclass)) type)
      (define-method (dimension (self metaclass)) size)
      (define-method (base (self metaclass)) (make-list size type))
      (define-method (components (self metaclass))
        (map (lambda (index) (cut get <> index)) (iota size))))))

(define-method (constructor (type <meta<llvmlist<>>>))
  "Get constructor for static size list"
  list)

(define-method (get (self <list>) index)
  "Get element of list"
  (list-ref self index))

(define-method (get (self <llvmlist<>>) index)
  "Element access for static size list in compiled code"
  (make (typecode self) #:value (lambda (fun) (list-ref ((get self) fun) index))))

(define (llvm-last self)
  "Get last element of list"
  (get self (1- (dimension self))))

(define (llvm-all-but-last self)
  "Get all but last element of a list"
  (apply llvmlist (map (lambda (index) (get self index)) (iota (1- (dimension self))))))

(define-class* <multiarray<>> <object> <meta<multiarray<>>> <class>
               (shape       #:init-keyword #:shape       #:getter shape      )
               (strides     #:init-keyword #:strides     #:getter strides    )
               (memory      #:init-keyword #:memory      #:getter memory     )
               (memory-base #:init-keyword #:memory-base #:getter memory-base))

(define (default-strides shape)
  "Compute strides for compact array"
  (map (compose (cut apply * <>) (cut list-head shape <>)) (iota (length shape))))

(define (ensure-default-strides self)
  "Create copy of array if it is not compact"
  (if (equal? (strides self) (default-strides (shape self)))
    self
    (duplicate self)))

(define-method (initialize (self <multiarray<>>) initargs)
  (let-keywords initargs #f (memory memory-base shape strides allocator)
    (let* [(allocator   (or allocator gc-malloc-pointerless))
           (memory      (or memory (allocator (apply * (size-of (typecode self)) shape))))
           (memory-base (or memory-base memory))
           (strides     (or strides (default-strides shape)))]
      (next-method self (list #:memory memory #:shape shape #:strides strides #:memory-base memory-base)))))

(define-method (typecode (self <multiarray<>>)) (typecode (class-of self)))

(define-method (dimensions (self <multiarray<>>)) (dimensions (class-of self)))

(define (multiarray type dim)
  "Define multi-dimensional array"
  (template-class (multiarray type dim) <multiarray<>>
    (lambda (class metaclass)
      (define-method (dimensions (self metaclass)) dim)
      (define-method (typecode  (self metaclass)) type))))

(define (roll self)
  (make (class-of self)
        #:shape       (attach (cdr (shape self)) (car (shape self)))
        #:strides     (attach (cdr (strides self)) (car (strides self)))
        #:memory      (memory self)
        #:memory-base (memory-base self)))

(define-class* <llvmarray<>> <structure> <meta<llvmarray<>>> <meta<structure>>)

(define-method (typecode (self <llvmarray<>>)) (typecode (class-of self)))

(define-method (dimensions (self <llvmarray<>>)) (dimensions (class-of self)))

(define-method (llvmarray type dim)
  "Define compiled multi-dimensional array"
  (template-class (llvmarray type dim) <llvmarray<>>
    (lambda (class metaclass)
      (define-method (dimensions (self metaclass)) dim)
      (define-method (typecode (self metaclass)) type)
      (define-method (base (self metaclass))
        (list (pointer type) (pointer type) (llvmlist <int> dim) (llvmlist <int> dim)))
      (define-method (constructor (self metaclass))
        (lambda (memory memory-base shape strides)
          (make (multiarray type dim)
                #:memory memory
                #:memory-base memory-base
                #:shape shape
                #:strides strides))))))

(component-accessors <llvmarray<>> <meta<llvmarray<>>> memory memory-base shape strides)

(define-method (equal? (a <void>) (b <void>))
  (equal? (get a) (get b)))

(define-method (base (type <meta<void>>))
  '())

(define-method (base (type <meta<scalar>>))
  (list type))

(define-method (foreign-type (type <meta<void>>))
  void)

(define-method (foreign-type (type <meta<bool>>))
  bool)

(define-method (foreign-type (type <meta<int<>>>))
  "Get foreign type for integer type"
  (- (* 2 (inexact->exact (/ (log (bits type)) (log 2)))) (if (signed? type) 2 3)))

(define-method (foreign-type (type <meta<float<>>>))
  "Get foreign type for floating-point type"
  (if (double-precision? type) double float))

(define-method (foreign-type (type <meta<pointer<>>>))
  "Get foreing type of pointer"
  int64)

(define-method (foreign-type (type <meta<llvmlist<>>>))
  "Get foreign type of static size list"
  int64)

(define-method (foreign-type (type <meta<llvmarray<>>>))
  "Get foreign type of multi-dimensional array"
  int64)

(define-method (size-of (type <meta<bool>>))
  1)

(define-method (size-of (type <meta<int<>>>))
  "Get size of integer values"
  (/ (bits type) 8))

(define-method (size-of (type <meta<float<>>>))
  "Get size of floating-point values"
  (if (double-precision? type) 8 4))

(define-method (size-of (type <meta<void>>))
  "Determine size of type"
  (apply + (map size-of (base type))))

(define-method (size-of (type <meta<pointer<>>>))
  "Size of pointer"
  8)

(define-method (native-type (value <boolean>) . args)
  (if (every boolean? args) <bool> (next-method)))

(define-method (native-type (value <integer>) . args)
  (if (every integer? args)
    (let [(lower (apply min (cons value args)))
          (upper (apply max (cons value args)))]
      (if (>= lower 0)
        (cond ((< upper (ash 1  8)) <ubyte>)
              ((< upper (ash 1 16)) <usint>)
              ((< upper (ash 1 32)) <uint> )
              ((< upper (ash 1 64)) <ulong>)
              (else (next-method)))
        (let [(nlower (max (lognot lower) upper))]
          (cond ((< nlower (ash 1  7)) <byte>)
                ((< nlower (ash 1 15)) <sint>)
                ((< nlower (ash 1 31)) <int> )
                ((< nlower (ash 1 63)) <long>)
                (else (next-method))))))
    (next-method)))

(define-method (native-type (value <real>) . args)
  (if (every real? args) <double> (next-method)))

(define-method (native-type (value <complex>) . args)
  (if (every complex? args) <complex<double>> (next-method)))

(define-method (native-type (value <rgb>) . args)
  (rgb (apply native-type
              (append-map (lambda (x) (if (is-a? x <rgb>) (list (red x) (green x) (blue x)) (list x)))
              (cons value args)))))

(define-method (native-type (value <list>))
  (llvmlist (apply native-type value) (length value)))

(define-method (native-type (value <multiarray<>>))
  (llvmarray (typecode value) (dimensions value)))

(define-method (unpack-value (self <meta<void>>) address)
  address)

(define-method (unpack-value (self <meta<bool>>) address)
  (not (equal? (pointer->bytevector (make-pointer address) (size-of self)) #vu8(0))))

(define-method (unpack-value (self <meta<int<>>>) (address <integer>))
  "Unpack integer stored in a byte vector"
  (let [(packed    (pointer->bytevector (make-pointer address) (size-of self)))
        (converter (if (signed? self) bytevector->sint-list bytevector->uint-list))]
    (car (converter packed (native-endianness) (bytevector-length packed)))))

(define-method (unpack-value (self <meta<float<>>>) (address <integer>))
  "Unpack floating-point value stored in a byte vector"
    (let [(packed    (pointer->bytevector (make-pointer address) (size-of self)))
          (converter (if (double-precision? self) bytevector-ieee-double-native-ref bytevector-ieee-single-native-ref))]
      (converter packed 0)))

(define-method (unpack-value (self <meta<pointer<>>>) address)
  (make-pointer (car (bytevector->sint-list (pointer->bytevector (make-pointer address) 8) (native-endianness) 8))))

(define-method (unpack-value (self <meta<structure>>) (address <integer>))
  "Unpack composite value stored in a byte vector"
  (apply (constructor self) (map (lambda (type offset) (unpack-value type (+ address offset)))
                                 (base self)
                                 (integral (cons 0 (all-but-last (map size-of (base self))))))))

(define-method (coerce (a <meta<int<>>>) (b <meta<int<>>>))
  "Type coercion for integers"
  (let* [(is-signed? (or (signed? a) (signed? b)))
         (to-signed  (lambda (t) (if (signed? t) t (integer (* 2 (bits t)) signed))))
         (adapt      (if (eq? (signed? a) (signed? b)) identity to-signed))]
    (integer (min 64 (max (bits (adapt a)) (bits (adapt b)))) (if is-signed? signed unsigned))))

(define-method (coerce (a <meta<float<>>>) (b <meta<float<>>>))
  "Coerce floating-point numbers"
  (if (double-precision? a) a b))

(define-method (coerce (a <meta<float<>>>) (b <meta<int<>>>))
  "Coerce floating-point number and integer"
  a)

(define-method (coerce (a <meta<int<>>>) (b <meta<float<>>>))
  "Coerce integer and floating-point number"
  b)

(define-method (coerce (a <meta<complex<>>>) (b <meta<complex<>>>))
  (complex (coerce (apply coerce (base a)) (apply coerce (base b)))))

(define-method (coerce (a <meta<scalar>>) (b <meta<complex<>>>))
  (complex (coerce a (apply coerce (base b)))))

(define-method (coerce (a <meta<complex<>>>) (b <meta<scalar>>))
  (complex (coerce (apply coerce (base a)) b)))

(define-method (coerce (a <meta<rgb<>>>) (b <meta<rgb<>>>))
  (rgb (reduce coerce #f (append (base a) (base b)))))

(define-method (coerce (a <meta<rgb<>>>) (b <meta<scalar>>))
  (rgb (reduce coerce #f (cons b (base a)))))

(define-method (coerce (a <meta<scalar>>) (b <meta<rgb<>>>))
  (rgb (reduce coerce #f (cons a (base b)))))

(define-method (coerce (a <meta<pointer<>>>) (b <meta<int<>>>))
  "Coerce pointers and integers"
  a)

(define-method (coerce (a <meta<pointer<>>>) (b <meta<pointer<>>>))
  "Coerce two pointers"
  (pointer (coerce (target a) (target b))))

(define-method (decompose-type (type <meta<scalar>>))
  "Decompose scalar type"
  (base type))

(define-method (decompose-type (type <meta<structure>>))
  "Decompose composite type"
  (append-map decompose-type (base type)))

(define-method (decompose-type (type <meta<pointer<>>>))
  (list <long>))

(define-method (decompose-argument (type <meta<scalar>>) value)
  "Decompose scalar value"
  (list value))

(define-method (decompose-argument (type <meta<bool>>) value)
  "Decompose boolean value"
  (list (if value 1 0)))

(define-method (decompose-argument (type <meta<structure>>) value)
  "Recursively decompose composite value"
  (append-map decompose-argument (base type) (map (cut <> value) (components type))))

(define-method (decompose-argument (type <meta<pointer<>>>) value)
  (list (pointer-address value)))

(define-method (decompose-result (type <meta<scalar>>) value)
  (list value))

(define-method (decompose-result (type <meta<structure>>) value)
  "Recursively decompose composite value"
  (append-map decompose-result (base type) (map (cut <> value) (components type))))

(define (compose-base base-types lst)
  (if (null? base-types)
    (cons (const '()) lst)
    (let* [(result (compose-content (car base-types) lst))
           (rest   (compose-base    (cdr base-types) (cdr result)))]
      (cons (lambda (fun) (cons ((car result) fun) ((car rest) fun))) (cdr rest)))))

(define-method (compose-content (type <meta<structure>>) lst)
  (compose-base (base type) lst))

(define-method (compose-content (type <meta<scalar>>) lst)
  (cons (car lst) (cdr lst)))

(define (compose-value type lst)
  "Compose a scalar value"
  (let [(content (compose-content type lst))]
    (cons (make type #:value (car content)) (cdr content))))

(define (compose-values types lst)
  "Compose multiple values"
  (if (null? types)
    '()
    (let [(result (compose-content (car types) lst))]
      (cons (make (car types) #:value (car result)) (compose-values (cdr types) (cdr result))))))

(define-class* <llvm> <object> <meta<llvm>> <class>
               (llvm-module #:init-keyword #:llvm-module))

(define-method (initialize (self <llvm>) initargs)
  (next-method self (list #:llvm-module (make-llvm-module-base))))

(define (make-llvm-module) (make <llvm>))

(define-method (destroy (self <llvm>)) (llvm-module-destroy (slot-ref self 'llvm-module)))

(define-class* <llvm-function> <object> <meta<llvm-function>> <class>
               (module         #:init-keyword #:module        )
               (name           #:init-keyword #:name          )
               (return-type    #:init-keyword #:return-type   )
               (llvm-function  #:init-keyword #:llvm-function )
               (argument-types #:init-keyword #:argument-types))

(define-method (initialize (self <llvm-function>) initargs)
  (let-keywords initargs #f (module return-type name argument-types)
    (let* [(fun (make-llvm-function (slot-ref module 'llvm-module)
                                   return-type
                                   name
                                   argument-types))
           (entry (make-llvm-basic-block fun "entry"))]
    (llvm-position-builder-at-end fun entry)
    (next-method self (list #:module         module
                            #:name           name
                            #:return-type    return-type
                            #:llvm-function  fun
                            #:argument-types argument-types)))))

(define (make-function module return-type name . argument-types)
  (make <llvm-function> #:module         module
                        #:return-type    return-type
                        #:name           name
                        #:argument-types argument-types))

(define-method (destroy (self <llvm-function>)) (llvm-function-destroy (slot-ref self 'llvm-function)))

(define-syntax-rule (memoize (arguments ...) body ...)
  (perfect-funcq 1 (lambda (arguments ...) body ...)))

(define* ((function-ret #:optional (result (lambda (fun) #f))) fun)
  (let [(llvm-function (slot-ref fun 'llvm-function))
        (return-value  (result fun))]
    (if return-value
      (llvm-function-return llvm-function return-value)
      (llvm-function-return-void llvm-function))))

(define (llvm-dump self) (llvm-dump-module (slot-ref self 'llvm-module)))

(define (llvm-compile self)
  (if (equal? "YES" (getenv "DEBUG")) (llvm-dump self))
  (llvm-verify-module (slot-ref self 'llvm-module))
  (llvm-compile-module (slot-ref self 'llvm-module)))

(define-syntax-rule (llvm-set value expression)
  (let [(result expression)]
    (set! value expression)
    (make <void> #:value (memoize (fun) ((get value) fun)))))

(define-syntax typed-let
  (lambda (x)
    (syntax-case x ()
      ((k [] instructions ...)
       #'(llvm-begin instructions ...))
      ((k [(name value) declarations ...] instructions ...)
       #'(let [(name #f)] (llvm-begin (llvm-set name value) (typed-let [declarations ...] instructions ...)))))))

(define (make-basic-block name)
  (memoize (fun) (make-llvm-basic-block (slot-ref fun 'llvm-function) name)))

(define (position-builder-at-end basic-block)
  (make <void> #:value (lambda (fun) (llvm-position-builder-at-end (slot-ref fun 'llvm-function) (basic-block fun)))))

(define (build-branch basic-block)
  (make <void> #:value (lambda (fun) (llvm-build-branch (slot-ref fun 'llvm-function) (basic-block fun)))))

(define (build-cond-branch condition block-then block-else)
  (make <void>
        #:value (lambda (fun)
                  (llvm-build-cond-branch (slot-ref fun 'llvm-function)
                                          ((get condition) fun)
                                          (block-then fun)
                                          (block-else fun)))))

(define (bool->int8 type)
  (if (eqv? type llvm-bool) int8 type))

(define (llvm-func llvm fun)
  (let [(pointer (llvm-get-function-address (slot-ref llvm 'llvm-module) (slot-ref fun 'name)))]
    (pointer->procedure (bool->int8 (slot-ref fun 'return-type)) pointer (map bool->int8 (slot-ref fun 'argument-types)))))

(define (make-constant type value)
  "Create a constant LLVM value"
  (memoize (fun) (make-llvm-constant type value)))

(define (make-constant-pointer address)
  "Create pointer constant"
  (make-constant llvm-int64 (pointer-address address)))

(define (get-type value)
  "Query type of LLVM value"
  (llvm-get-type value))

(define (llvm-fetch type address)
  "Generate code for reading value from memory"
  (memoize (fun) (llvm-build-load (slot-ref fun 'llvm-function) type (address fun))))

(define (llvm-store type value address)
  "Generate code for writing value to memory"
  (memoize (fun)
    (llvm-build-store (slot-ref fun 'llvm-function) type (value fun) (address fun))))

(define ((function-param index) fun)
  "Get value of INDEXth function parameter"
  (llvm-get-param (slot-ref fun 'llvm-function) index))

(define-syntax-rule (define-llvm-unary function delegate)
  (define (function value)
    (memoize (fun) (delegate (slot-ref fun 'llvm-function) (value fun)))))

(define-llvm-unary llvm-neg  llvm-build-neg )
(define-llvm-unary llvm-fneg llvm-build-fneg)
(define-llvm-unary llvm-not  llvm-build-not )

(define-syntax-rule (define-llvm-binary function delegate)
  (define (function value-a value-b)
    (memoize (fun) (delegate (slot-ref fun 'llvm-function) (value-a fun) (value-b fun)))))

(define-llvm-binary llvm-add  llvm-build-add )
(define-llvm-binary llvm-fadd llvm-build-fadd)
(define-llvm-binary llvm-sub  llvm-build-sub )
(define-llvm-binary llvm-fsub llvm-build-fsub)
(define-llvm-binary llvm-mul  llvm-build-mul )
(define-llvm-binary llvm-fmul llvm-build-fmul)

(define ((build-integer-cmp predicate) fun value-a value-b)
  (llvm-build-integer-cmp fun predicate value-a value-b))

(define ((build-float-cmp predicate) fun value-a value-b)
  (llvm-build-float-cmp fun predicate value-a value-b))

; integer comparisons
(define-llvm-binary llvm-s-lt (build-integer-cmp llvm-int-slt))
(define-llvm-binary llvm-u-lt (build-integer-cmp llvm-int-ult))
(define-llvm-binary llvm-s-le (build-integer-cmp llvm-int-sle))
(define-llvm-binary llvm-u-le (build-integer-cmp llvm-int-ule))
(define-llvm-binary llvm-s-gt (build-integer-cmp llvm-int-sgt))
(define-llvm-binary llvm-u-gt (build-integer-cmp llvm-int-ugt))
(define-llvm-binary llvm-s-ge (build-integer-cmp llvm-int-sge))
(define-llvm-binary llvm-u-ge (build-integer-cmp llvm-int-uge))
(define-llvm-binary llvm-eq   (build-integer-cmp llvm-int-eq ))
(define-llvm-binary llvm-ne   (build-integer-cmp llvm-int-ne ))

; floating point comparisons
(define-llvm-binary llvm-f-lt (build-float-cmp llvm-real-lt))
(define-llvm-binary llvm-f-le (build-float-cmp llvm-real-le))
(define-llvm-binary llvm-f-gt (build-float-cmp llvm-real-gt))
(define-llvm-binary llvm-f-ge (build-float-cmp llvm-real-ge))
(define-llvm-binary llvm-f-eq (build-float-cmp llvm-real-eq))
(define-llvm-binary llvm-f-ne (build-float-cmp llvm-real-ne))

(define-syntax-rule (define-llvm-cast function delegate)
  (define (function type value)
    (memoize (fun) (delegate (slot-ref fun 'llvm-function) type (value fun)))))

(define-llvm-cast llvm-trunc    llvm-build-trunc   )
(define-llvm-cast llvm-sext     llvm-build-sext    )
(define-llvm-cast llvm-zext     llvm-build-zext    )
(define-llvm-cast llvm-fp-cast  llvm-build-fp-cast )
(define-llvm-cast llvm-fp-to-si llvm-build-fp-to-si)
(define-llvm-cast llvm-fp-to-ui llvm-build-fp-to-ui)
(define-llvm-cast llvm-si-to-fp llvm-build-si-to-fp)
(define-llvm-cast llvm-ui-to-fp llvm-build-ui-to-fp)

(define module-list '())

(define (return . args)
  (make <void> #:value (apply function-ret (map get args))))

(define (replace-bool-with-int8 type)
  (if (eqv? type llvm-bool) int8 type))

(define (llvm-wrap foreign-types function)
  "Convenience wrapper for compiling JIT functions"
  (let* [(mod         (make-llvm-module))
         (arguments   (map function-param (iota (length foreign-types))))
         (result      (apply function arguments))
         (return-type (car result))
         (expression  (cdr result))
         (fun         (apply make-function mod return-type "wrapped" foreign-types)) ]
    (expression fun)
    (llvm-compile mod)
    (set! module-list (cons mod module-list))
    (llvm-func mod fun)))

(define-method (to-type (cls <meta<int<>>>) (value <int<>>))
  "Integer conversions"
  (let [(conversion (if (> (bits cls) (bits value)) (if (signed? value) llvm-sext llvm-zext) llvm-trunc))]
    (make cls #:value (conversion (foreign-type cls) (get value)))))

(define-method (to-type (cls <meta<float<>>>) (value <float<>>))
  "Floating-point conversions"
  (make cls #:value (llvm-fp-cast (foreign-type cls) (get value))))

(define-method (to-type (cls <meta<float<>>>) (value <int<>>))
  "Convert integer to floating-point"
  (let [(conversion (if (signed? value) llvm-si-to-fp llvm-ui-to-fp))]
    (make cls #:value (conversion (foreign-type cls) (get value)))))

(define-method (to-type (cls <meta<int<>>>) (value <float<>>))
  "Floating-point to integer conversion"
  (let [(conversion (if (signed? cls) llvm-fp-to-si llvm-fp-to-ui))]
    (make cls #:value (conversion (foreign-type cls) (get value)))))

(define-method (to-type (cls <meta<complex<>>>) (value <scalar>))
  (complex value (typed-constant (class-of value) 0)))

(define-method (to-type (cls <meta<complex<>>>) (value <complex<>>))
  (complex (to-type (car  (base cls)) (real-part value))
           (to-type (cadr (base cls)) (imag-part value))))

(define-method (to-type (cls <meta<pointer<>>>) (value <pointer<>>))
  "Typecast pointer"
  (make cls #:value (get value)))

(define-method (to-type (cls <meta<pointer<>>>) (value <int<>>))
  "Convert integer to pointer"
  (make cls #:value (get (to-type <long> value))))

(define-method (to-type (cls <meta<int<>>>) (value <pointer<>>))
  "Convert pointer to integer"
  (make cls #:value (get value)))

(define-syntax-rule (define-unary-operation type operation delegate)
  (define-method (operation (value type))
    (make (class-of value) #:value (delegate (get value)))))

(define-method (+ (self <scalar>)) self)
(define-method (* (self <scalar>)) self)
(define-unary-operation <int<>>   - llvm-neg )
(define-unary-operation <float<>> - llvm-fneg)
(define-unary-operation <int<>>   ~ llvm-not )

(define-syntax-rule (define-binary-operation type-a type-b type-map operation delegate)
  (define-method (operation (value-a type-a) (value-b type-b))
    (let* [(target  (coerce (class-of value-a) (class-of value-b)))
           (adapt-a (to-type target value-a ))
           (adapt-b (to-type target value-b))]
      (make (type-map target) #:value ((delegate target) (get adapt-a) (get adapt-b)))))); TODO: remove redundant code below

(define-syntax-rule (define-op-with-constant type operation)
  (begin
    (define-method (operation (value-a type) (value-b <complex>))
      (operation value-a (typed-constant (native-type value-b) value-b)))
    (define-method (operation (value-a <complex>) (value-b type))
      (operation (typed-constant (native-type value-a) value-a) value-b))))

(define-syntax-rule (define-binary-delegation type-map operation delegate float-delegate)
  (begin
    (define-binary-operation <int<>>     <int<>>   type-map operation delegate )
    (define-binary-operation <float<>>   <int<>>   type-map operation float-delegate)
    (define-binary-operation <int<>>     <float<>> type-map operation float-delegate)
    (define-binary-operation <float<>>   <float<>> type-map operation float-delegate)
    (define-binary-operation <pointer<>> <int<>>   type-map operation delegate )
    (define-binary-operation <pointer<>> <pointer<>>   type-map operation delegate )
    (define-op-with-constant <void> operation)))

(define-binary-delegation identity + (const llvm-add) (const llvm-fadd))
(define-binary-delegation identity - (const llvm-sub) (const llvm-fsub))
(define-binary-delegation identity * (const llvm-mul) (const llvm-fmul))

(define-binary-delegation (const <bool>) lt (lambda (target) (if (signed? target) llvm-s-lt llvm-u-lt)) (const llvm-f-lt))
(define-binary-delegation (const <bool>) le (lambda (target) (if (signed? target) llvm-s-le llvm-u-le)) (const llvm-f-le))
(define-binary-delegation (const <bool>) gt (lambda (target) (if (signed? target) llvm-s-gt llvm-u-gt)) (const llvm-f-gt))
(define-binary-delegation (const <bool>) ge (lambda (target) (if (signed? target) llvm-s-ge llvm-u-ge)) (const llvm-f-ge))
(define-binary-delegation (const <bool>) eq (lambda (target) llvm-eq) (const llvm-f-eq))
(define-binary-delegation (const <bool>) ne (lambda (target) llvm-ne) (const llvm-f-ne))

(define (construct-object class args)
  (make class #:value (memoize (fun) (map (lambda (component) ((get component) fun)) args))))

(define-syntax-rule (define-mixed-constructor name)
  (define-method (name (arg <void>) . args)
    (construct-object (apply name (map class-of (cons arg args))) (cons arg args))))

(define-syntax-rule (define-uniform-constructor name)
  (define-method (name (arg <void>) . args)
    (let* [(target  (reduce coerce #f (map class-of (cons arg args))))
           (adapted (map (cut to-type target <>) (cons arg args)))]
      (construct-object (name target) adapted))))

(define-uniform-constructor complex)

(define-uniform-constructor rgb)

(define-method (real-part (value <scalar>)) value)
(define-method (imag-part (value <scalar>)) (typed-constant (class-of value) 0))

(define-method (- (value <complex<>>))
  (complex (- (real-part value)) (- (imag-part value))))

(define-method (+ (value-a <complex<>>) (value-b <complex<>>))
  (complex (+ (real-part value-a) (real-part value-b)) (+ (imag-part value-a) (imag-part value-b))))

(define-method (+ (value-a <complex<>>) (value-b <scalar>))
  (complex (+ (real-part value-a) value-b) (imag-part value-a)))

(define-method (+ (value-a <scalar>) (value-b <complex<>>))
  (complex (+ value-a (real-part value-b)) (imag-part value-b)))

(define-method (- (value-a <complex<>>) (value-b <complex<>>))
  (complex (- (real-part value-a) (real-part value-b)) (- (imag-part value-a) (imag-part value-b))))

(define-method (- (value-a <complex<>>) (value-b <scalar>))
  (complex (- (real-part value-a) value-b) (imag-part value-a)))

(define-method (- (value-a <scalar>) (value-b <complex<>>))
  (complex (- value-a (real-part value-b)) (- (imag-part value-b))))

(define (llvm-begin instruction . instructions)
  (if (null? instructions)
    instruction
    (let [(result (apply llvm-begin instructions))]
      (make (class-of result) #:value (memoize (fun) ((get instruction) fun) ((get result) fun))))))

(define-method (typed-constant (type <meta<scalar>>) value)
  (make type #:value (make-constant (foreign-type type) value)))

(define-method (typed-constant (type <meta<void>>) value)
  (apply (build type)
         (map (lambda (type component) (typed-constant type (component value)))
              (base type)
              (components type))))

(define (typed-pointer target value)
  (make (pointer target) #:value (make-constant-pointer value)))

(define-method (store (ptr <pointer<>>) (value <scalar>))
  (let [(type (target (class-of ptr)))]
    (make <void> #:value (llvm-store (foreign-type type) (get (to-type type value)) (get ptr)))))

(define-method (store (ptr <pointer<>>) (value <structure>))
  (let [(type (target (class-of ptr)))]
    (apply llvm-begin
      (map (lambda (component type offset) (store (to-type (pointer type) (+ ptr offset)) component))
           (decompose-result type value)
           (decompose-type type)
           (integral (cons 0 (all-but-last (map size-of (decompose-type type)))))))))

(define-method (fetch (type <meta<scalar>>) ptr)
  (make type #:value (llvm-fetch (foreign-type type) (get ptr))))

(define-method (fetch (type <meta<structure>>) ptr)
  (apply (build type) (map (lambda (type offset) (fetch type (+ ptr offset)))
                           (base type)
                           (integral (cons 0 (all-but-last (map size-of (base type))))))))

(define-method (fetch (ptr <pointer<>>))
  (fetch (target (class-of ptr)) ptr))

(define-method (prepare-return (result <void>) memory)
  "Generate return statement for void"
  (llvm-begin result (return)))

(define-method (prepare-return (result <structure>) memory)
  "Generate return statement for composite value"
  (llvm-begin (store (to-type (pointer (class-of result)) memory) result) (return memory)))

(define-method (prepare-return (result <scalar>) memory)
  "Generate return statement for boolean, integer, or floating-point number"
  (return result))

(define-method (finish-return type result)
  "Provide composite return value"
  (unpack-value type result))

(define-method (finish-return (type <meta<scalar>>) result)
  "Provide integer or floating-point value"
  result)

(define-method (finish-return (type <meta<bool>>) result)
  "Provide boolean return value"
  (not (zero? result)))

(define-method (finish-return (type <meta<pointer<>>>) result)
  "Provide pointer return value"
  (make-pointer result))

(define (llvm-typed argument-types function)
  "Infer types and compile function"
  (let* [(result-type #f)
         (fun (llvm-wrap (cons llvm-int64 (map foreign-type (append-map decompose-type argument-types)))
               (lambda arguments
                 (let* [(arguments-typed (compose-values (cons <pointer<>> argument-types) arguments))
                        (expression      (apply function (cdr arguments-typed)))]
                   (set! result-type (class-of expression))
                   (cons (foreign-type result-type) (get (prepare-return expression (car arguments-typed))))))))]
    (lambda args
      (let [(memory (make-bytevector (size-of result-type)))]
        (finish-return
          result-type
          (apply fun
            (cons (pointer-address (bytevector->pointer memory))
                  (append-map decompose-argument argument-types args))))))))

(define (llvm-call return-type function-name argument-types args)
  "Call a C function"
  (memoize (fun)
    (llvm-build-call (slot-ref fun 'llvm-function)
                     (slot-ref (slot-ref fun 'module) 'llvm-module)
                     return-type
                     function-name
                     argument-types
                     (map (lambda (arg) (arg fun)) args))))

(define (typed-call return-type function-name argument-types args)
  "Call a C function"
  (make return-type
        #:value (llvm-call (foreign-type return-type)
                           function-name
                           (map foreign-type argument-types)
                           (map get args))))

(define-method (llvm-if condition value-if value-else)
  (let [(target (coerce (class-of value-if) (class-of value-else)))]
    (llvm-if target condition value-if value-else)))

(define-method (llvm-if (target <meta<scalar>>) condition value-if value-else)
  (make target #:value (lambda (fun)
    (llvm-build-select (slot-ref fun 'llvm-function)
                       ((get condition) fun)
                       ((get (to-type target value-if  )) fun)
                       ((get (to-type target value-else)) fun)))))

(define-method (llvm-if (target <meta<structure>>) condition value-if value-else)
  (let [(args (map (lambda (component) (llvm-if condition (component value-if) (component value-else)))
                   (components target)))]
    (construct-object target args)))

(define-method (typed-alloca (type <meta<scalar>>)) (make (pointer type) #:value (memoize (fun) (llvm-build-alloca (slot-ref fun 'llvm-function) (foreign-type type)))))

(define-syntax-rule (llvm-while condition body ...)
  (let [(block-while (make-basic-block "while"))
        (block-body  (make-basic-block "body"))
        (block-end   (make-basic-block "endwhile"))]
    (llvm-begin
      (build-branch block-while)
      (position-builder-at-end block-while)
      (build-cond-branch condition block-body block-end)
      (position-builder-at-end block-body)
      body ...
      (build-branch block-while)
      (position-builder-at-end block-end))))

(define-method (llvmlist)
  "Empty compiled list defaults to integer list"
  (make (llvmlist <int> 0) #:value (lambda (fun) #f)))

(define-method (llvmlist (arg <void>) . args)
  "Create uniform typed list of values"
  (let [(args (cons arg args))]
    (make (llvmlist (reduce coerce #f (map class-of args)) (length args))
          #:value (memoize (fun) (map (lambda (arg) ((get arg) fun)) args)))))

(define-method (llvmlist (arg <integer>) . args)
  "List starting with integer defaults to integer list"
  (apply llvmlist (typed-constant <int> arg) args))

(define-method (llvmarray memory memory-base shape strides)
  (make (llvmarray (target memory) (dimension shape))
        #:value (memoize (fun) (list ((get memory) fun)
                                     ((get memory-base) fun)
                                     ((get shape) fun)
                                     ((get strides) fun)))))

(define (element self indices dim)
  (reduce-right +
                #f
                (cons (memory self)
                  (map (lambda (index i) (* index (get (strides self) i) (size-of (typecode self))))
                       indices
                       (iota (length indices) dim)))))

(define-method (get (self <multiarray<>>) . indices)
  (let* [(dim (- (dimensions self) (length indices)))
         (fun (if (zero? dim)
                (lambda (self . indices) (fetch (element self indices dim) ))
                (lambda (self . indices)
                  (llvmarray (element self indices dim)
                             (memory-base self)
                             (apply llvmlist (map (lambda (index) (get (shape self) index)) (iota dim)))
                             (apply llvmlist (map (lambda (index) (get (strides self) index)) (iota dim)))))))]
    (add-method! get
                 (make <method>
                       #:specializers (cons (class-of self) (make-list (length indices) <integer>))
                       #:procedure (llvm-typed (cons (native-type self) (make-list (length indices) <int>)) fun)))
    (apply get self indices)))

(define-method (set (self <multiarray<>>) . args)
  (let* [(indices (all-but-last args))
         (value   (last args))
         (dim     (- (dimensions self) (length indices)))
         (fun (lambda (self . args)
           (store (element self (all-but-last args) dim) (last args))))]
    (add-method! set
                 (make <method>
                       #:specializers (attach (cons (class-of self) (make-list (length indices) <integer>)) <top>)
                       #:procedure (llvm-typed (attach (cons (native-type self) (make-list (length indices) <int>)) (typecode self)) fun)))
    (apply set self args)))

(define (to-list self)
  (let [(indices (iota (last (shape self))))]
    (if (> (dimensions self) 1)
      (map (lambda (index) (to-list (get self index))) indices)
      (map (cut get self <>) indices))))

(define-method (shape self)
  "Shape of scalar"
  '())

(define-method (shape (self <list>))
  "Shape of list"
  (attach (shape (car self)) (length self)))

(define-method (store (self <multiarray<>>) (value <list>))
  (if (<= (dimensions self) 1)
    (for-each (lambda (index value) (set self index value)) (iota (length value)) value)
    (for-each (lambda (index value) (store (get self index) value)) (iota (length value)) value)))

(define-method (to-array (typecode <meta<void>>) (lst <list>))
  "Convert list to array of specified type"
  (let* [(shp    (shape lst))
         (result (make (multiarray typecode (length shp)) #:shape shp))]
    (store result lst)
    result))

(define-method (to-array (lst <list>))
  "Convert list to array"
  (to-array (apply native-type (flatten lst)) lst))

(define-syntax-rule (arr arg args ...)
  (if (is-a? (quote arg) <symbol>)
    (to-array arg '(args ...))
    (to-array '(arg args ...))))

(define (print-elements self port offset remaining)
  (if (< offset (last (shape self)))
    (begin
      (if (not (zero? offset))
        (display " " port))
      (let [(text (call-with-output-string (lambda (port) (write (get self offset) port))))]
        (if (>= (string-length text) remaining)
          (display "..." port)
          (begin
            (display text port)
            (print-elements self port (1+ offset) (- remaining 1 (string-length text)))))))))

(define (print-data self port depth line-counter cont)
  (let* [(dim       (dimensions self))
         (separator (apply string-append "\n" (make-list depth " ")))]
    (if (> dim 1)
      (begin
        (display "(" port)
        (for-each
          (lambda (index)
            (if (not (zero? index)) (display separator port))
            (print-data (get self index) port (1+ depth) line-counter cont))
          (iota (last (shape self)))))
      (if (> (line-counter) 10)
        (begin (display "..." port) (cont))
        (begin (display "(" port) (print-elements self port 0 80))))
    (display ")" port)))

(define-method (write (self <multiarray<>>) port)
  (let* [(lines         0)
         (line-counter (lambda () (set! lines (1+ lines)) lines))]
    (call/cc
      (lambda (cont)
        (format port "#~a:~%" (class-name (class-of self)))
        (if (zero? (dimensions self))
          (write (get self) port)
          (print-data self port 1 line-counter cont))))))

(define (rebase self p)
  "Use the specified pointer to rebase the array"
  (llvmarray p (memory-base self) (shape self) (strides self)))

(define (project self)
  "Drop last dimension of array"
  (llvmarray (memory self) (memory-base self) (llvm-all-but-last (shape self)) (llvm-all-but-last (strides self))))

(define-method (fetch (self <llvmarray<>>))
  (if (zero? (dimensions self)) (fetch (memory self)) self))

(define (unary-loop delegate result a)
  "Compile loop for unary array operation"
  (if (>= (dimensions result) 1)
    (typed-let [(p (typed-alloca (pointer (typecode result))))
                (q (typed-alloca (pointer (typecode a))))]
      (store p (memory result))
      (store q (memory a))
      (llvm-while (ne (fetch p) (+ (memory result) (* (llvm-last (shape result)) (llvm-last (strides result)) (size-of (typecode result)))))
        (unary-loop delegate (project (rebase result (fetch p))) (fetch (project (rebase a (fetch q)))))
        (store p (+ (fetch p) (* (llvm-last (strides result)) (size-of (typecode result)))))
        (store q (+ (fetch q) (* (llvm-last (strides a)) (size-of (typecode a)))))))
    (store (memory result) (delegate a))))

(define (binary-loop op result a b)
  (typed-let [(p (typed-alloca (pointer (typecode result))))
              (q (typed-alloca (pointer (typecode a))))
              (r (typed-alloca (pointer (typecode b))))]
    (store p (memory result))
    (store q (memory a))
    (store r (memory b))
    (llvm-while (ne (fetch p) (+ (memory result) (* (llvm-last (shape result)) (llvm-last (strides result)) (size-of (typecode result)))))
      (if (> (dimensions result) 1)
        (if (> (dimensions b) 1)
          (if (> (dimensions a) 1)
            (binary-loop
              op
              (project (rebase result (fetch p)))
              (project (rebase a (fetch q)))
              (project (rebase b (fetch r))))
            (let [(a (fetch (fetch q)))]
              (unary-loop
                (lambda (value) (op a value))
                (project (rebase result (fetch p)))
                (project (rebase b (fetch r))))))
          (let [(b  (fetch (fetch r)))]
            (unary-loop
              (lambda (value) (op value b))
              (project (rebase result (fetch p)))
              (project (rebase a (fetch q))))))
        (store (fetch p) (op (fetch (fetch q)) (fetch (fetch r)))))
      (store p (+ (fetch p) (* (llvm-last (strides result)) (size-of (typecode result)))))
      (store q (+ (fetch q) (* (llvm-last (strides a)) (size-of (typecode a)))))
      (store r (+ (fetch r) (* (llvm-last (strides b)) (size-of (typecode b))))))))

(define (compute-strides shape)
  "Compile code for computing strides"
  (apply llvmlist
         (map (lambda (index) (apply * (list-head (map (cut get shape <>) (iota (dimension shape))) index)))
              (iota (dimension shape)))))

(define (allocate-array typecode shape)
  (typed-let [(size    (apply * (size-of typecode) (map (cut get shape <>) (iota (dimension shape)))))
              (ptr     (typed-call (pointer typecode) "scm_gc_malloc_pointerless" (list <int>) (list size)))
              (strides (compute-strides shape))]
    (llvmarray ptr ptr shape strides)))

(define-syntax-rule (define-unary-array-op op delegate)
  (begin
    (define-method (op (self <llvmarray<>>))
      (typed-let [(result (allocate-array (typecode self) (shape self)))]
        (unary-loop delegate result self)
        result))
    (define-method (op (self <meta<void>>))
      (let [(fun (llvm-typed (list self) op))]
        (add-method! op (make <method> #:specializers (list (class-of self)) #:procedure (lambda args fun))))
        (op self))
    (define-method (op (self <multiarray<>>)) ((op (native-type self)) self))))

(define-unary-array-op -         -       )
(define-unary-array-op ~         ~       )
(define-unary-array-op duplicate identity)

(define-syntax-rule (define-binary-array-op op)
  (begin
    (define-method (op (a <llvmarray<>>) (b <llvmarray<>>))
      (typed-let [(result (allocate-array (coerce (typecode a) (typecode b))
                          (if (>= (dimensions a) (dimensions b)) (shape a) (shape b))))]
        (binary-loop op result a b)
        result))
    (define-method (op (a <llvmarray<>>) (b <void>))
      (typed-let [(result (allocate-array (coerce (typecode a) (class-of b)) (shape a)))]
        (unary-loop (lambda (value) (op value b)) result a)
        result))
    (define-method (op (a <void>) (b <llvmarray<>>))
      (typed-let [(result (allocate-array (coerce (class-of a) (typecode b)) (shape b)))]
        (unary-loop (lambda (value) (op a value)) result b)
        result))
    (define-method (op (a <meta<void>>) (b <meta<void>>))
      (let [(fun (llvm-typed (list a b) op))]
        (add-method! op (make <method> #:specializers (list (class-of a) (class-of b)) #:procedure (lambda args fun)))
        (op a b)))
    (define-method (op (a <multiarray<>>) b) ((op (native-type a) (native-type b)) a b))
    (define-method (op a (b <multiarray<>>)) ((op (native-type a) (native-type b)) a b))))

(define-binary-array-op +)
(define-binary-array-op -)
(define-binary-array-op *)

(define-generic read-image)
(define-generic write-image)
(define-generic read-audio)
(define-generic write-audio)
(define-generic rate)
(define-generic channels)
