(define-module (aiscm op)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (aiscm util)
  #:use-module (aiscm mem)
  #:use-module (aiscm jit)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm sequence)
  #:export (fill)
  #:re-export (+ -))
(define ctx (make <jit-context>))

(define-method (content (x <element>)) (get-value x))
(define-method (content (x <sequence<>>))
  (list ((compose pointer-address get-memory get-value) x) (size x)))

(define-method (types (x <meta<element>>)) x)
(define-method (types (x <meta<sequence<>>>))
  (list <long> <long>))

(define-method (arg (type <meta<sequence<>>>) (pool <pool>))
  (let [(value (arg <long> pool))
        (size  (arg <long> pool))]
    (make type #:value value #:size size)))

; (arg <sequence<>> pool) ?

(define-method (+ (a <element>)) a)
(define-method (+ (a <element>) (b <element>))
  (let* [(ca   (class-of a))
         (cb   (class-of b))
         (cr   (coerce ca cb))
         (pool (make <pool>))
         (code (asm ctx
                    cr
                    (env pool
                         [(a (arg cr pool))
                          (b (arg cr pool))
                          (r (reg cr pool))]
                         (MOV r a)
                         (ADD r b))
                    (flatten (map types (list ca cb)))))
         (proc (lambda (a b) (make cr #:value (apply code (flatten (map content (list a b)))))))]
    (add-method! + (make <method> #:specializers (list ca cb) #:procedure proc))
    (+ a b)))
(define-method (+ (a <element>) (b <integer>))
  (+ a (make (match b) #:value b)))
(define-method (+ (a <integer>) (b <element>))
  (+ (make (match a) #:value a) b))
(define-method (+ (a <sequence<>>) (b <element>))
  (let* [(ca   (class-of a))
         (cb   (class-of b))
         (cr   (coerce ca cb))
         (ta   (typecode ca))
         (tr   (typecode cr))
         (pool (make <pool>))
         (code (asm ctx
                    <null>
                    (env pool
                         [(r    (arg cr pool))
                          (a    (arg ca pool))
                          (b    (arg tr pool))
                          (v    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr (get-value r) (car (shape r))))
                         (CMP (get-value r) pend)
                         (JE 'return)
                         'loop
                         ((if (eq? ta tr) MOV (if (signed? ta) MOVSX MOVZX)) v (ptr ta (get-value a)))
                         (ADD v b)
                         (MOV (ptr tr (get-value r)) v)
                         (ADD (get-value r) (size-of tr))
                         (ADD (get-value a) (size-of ta))
                         (CMP pend (get-value r))
                         (JNE 'loop)
                         'return)
                    (flatten (map types (list cr ca cb)))))
         (proc (lambda (a b)
                 (let [(r (make cr #:size (size a)))]
                   (apply code (flatten (map content (list r a b))))
                   r)))]
    (add-method! + (make <method> #:specializers (list ca cb) #:procedure proc))
    (+ a b)))
(define-method (+ (a <element>) (b <sequence<>>))
  (let* [(ca   (class-of a))
         (cb   (class-of b))
         (cr   (coerce ca cb))
         (tb   (typecode cb))
         (tr   (typecode cr))
         (pool (make <pool>))
         (code (asm ctx
                    <null>
                    (env pool
                         [(r    (arg cr pool))
                          (a    (arg tr pool))
                          (b    (arg cb pool))
                          (v    (reg tr pool))
                          (w    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr (get-value r) (car (shape r))))
                         (CMP (get-value r) pend)
                         (JE 'return)
                         'loop
                         (MOV v a)
                         ((if (eq? tb tr) MOV (if (signed? tb) MOVSX MOVZX)) w (ptr tb (get-value b)))
                         (ADD v w)
                         (MOV (ptr tr (get-value r)) v)
                         (ADD (get-value r) (size-of tr))
                         (ADD (get-value b) (size-of tb))
                         (CMP pend (get-value r))
                         (JNE 'loop)
                         'return)
                    (flatten (map types (list cr ca cb)))))
         (proc (lambda (a b)
                 (let [(r (make cr #:size (size b)))]
                   (apply code (flatten (map content (list r a b))))
                   r)))]
    (add-method! + (make <method> #:specializers (list ca cb) #:procedure proc))
    (+ a b)))
(define-method (+ (a <sequence<>>) (b <sequence<>>))
  (let* [(ca   (class-of a))
         (cb   (class-of b))
         (cr   (coerce ca cb))
         (ta   (typecode ca))
         (tb   (typecode cb))
         (tr   (typecode cr))
         (pool (make <pool>))
         (code (asm ctx
                    <null>
                    (env pool
                         [(r    (arg cr pool))
                          (a    (arg ca pool))
                          (b    (arg cb pool))
                          (v    (reg tr pool))
                          (w    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr (get-value r) (car (shape r))))
                         (CMP (get-value r) pend)
                         (JE 'return)
                         'loop
                         ((if (eq? ta tr) MOV (if (signed? ta) MOVSX MOVZX)) v (ptr ta (get-value a)))
                         (if (eq? tb tr)
                           (ADD v (ptr tb (get-value b)))
                           (append
                             ((if (signed? tb) MOVSX MOVZX) w (ptr tb (get-value b)))
                             (ADD v w)))
                         (MOV (ptr tr (get-value r)) v)
                         (ADD (get-value r) (size-of tr))
                         (ADD (get-value a) (size-of ta))
                         (ADD (get-value b) (size-of tb))
                         (CMP pend (get-value r))
                         (JNE 'loop)
                         'return)
                    (flatten (map types (list cr ca cb)))))
         (proc (lambda (a b)
                 (let [(r (make cr #:size (size a)))]
                   (apply code (flatten (map content (list r a b))))
                   r)))]
    (add-method! + (make <method> #:specializers (list ca cb) #:procedure proc))
    (+ a b)))

(define-method (- (a <element>))
  (let* [(ca   (class-of a))
         (cr   ca)
         (pool (make <pool>))
         (code (asm ctx
                    cr
                    (env pool
                         [(a (arg cr pool))
                          (r (reg cr pool))]
                         (MOV r a)
                         (NEG r))
                    (flatten (map types (list ca)))))
         (proc (lambda (a) (make cr #:value (apply code (flatten (map content (list a)))))))]
    (add-method! - (make <method> #:specializers (list ca) #:procedure proc))
    (- a)))
(define-method (- (a <sequence<>>))
  (let* [(ca   (class-of a))
         (ta   (typecode ca))
         (cr   ca)
         (tr   (typecode cr))
         (pool (make <pool>))
         (code (asm ctx
                    <null>
                    (env pool
                         [(r    (arg cr pool))
                          (a    (arg ca pool))
                          (v    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr (get-value r) (car (shape r))))
                         (CMP (get-value r) pend)
                         (JE 'return)
                         'loop
                         (MOV v (ptr ta (get-value a)))
                         (NEG v)
                         (MOV (ptr tr (get-value r)) v)
                         (ADD (get-value r) (size-of ta))
                         (ADD (get-value a) (size-of tr))
                         (CMP pend (get-value r))
                         (JNE 'loop)
                         'return)
                    (flatten (map types (list cr ca)))))
         (proc (lambda (a)
                 (let [(r (make cr #:size (size a)))]
                   (apply code (flatten (map content (list r a))))
                   r)))]
    (add-method! - (make <method> #:specializers (list ca) #:procedure proc))
    (- a)))
(define-method (- (a <element>) (b <element>))
  (let* [(ca   (class-of a))
         (cb   (class-of b))
         (cr   (coerce ca cb))
         (pool (make <pool>))
         (code (asm ctx
                    cr
                    (env pool
                         [(a (arg cr pool))
                          (b (arg cr pool))
                          (r (reg cr pool))]
                         (MOV r a)
                         (SUB r b))
                    (flatten (map types (list ca cb)))))
         (proc (lambda (a b) (make cr #:value (apply code (flatten (map content (list a b)))))))]
    (add-method! - (make <method> #:specializers (list ca cb) #:procedure proc))
    (- a b)))
(define-method (- (a <element>) (b <integer>))
  (- a (make (match b) #:value b)))
(define-method (- (a <integer>) (b <element>))
  (- (make (match a) #:value a) b))
(define-method (- (a <sequence<>>) (b <element>))
  (let* [(ca   (class-of a))
         (cb   (class-of b))
         (cr   (coerce ca cb))
         (ta   (typecode ca))
         (tr   (typecode cr))
         (pool (make <pool>))
         (code (asm ctx
                    <null>
                    (env pool
                         [(r    (arg cr pool))
                          (a    (arg ca pool))
                          (b    (arg tr pool))
                          (v    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr (get-value r) (car (shape r))))
                         (CMP (get-value r) pend)
                         (JE 'return)
                         'loop
                         ((if (eq? ta tr) MOV (if (signed? ta) MOVSX MOVZX)) v (ptr ta (get-value a)))
                         (SUB v b)
                         (MOV (ptr tr (get-value r)) v)
                         (ADD (get-value r) (size-of tr))
                         (ADD (get-value a) (size-of ta))
                         (CMP pend (get-value r))
                         (JNE 'loop)
                         'return)
                    (flatten (map types (list cr ca cb)))))
         (proc (lambda (a b)
                 (let [(r (make cr #:size (size a)))]
                   (apply code (flatten (map content (list r a b))))
                   r)))]
    (add-method! - (make <method> #:specializers (list ca cb) #:procedure proc))
    (- a b)))
(define-method (- (a <element>) (b <sequence<>>))
  (let* [(ca   (class-of a))
         (cb   (class-of b))
         (cr   (coerce ca cb))
         (tb   (typecode cb))
         (tr   (typecode cr))
         (pool (make <pool>))
         (code (asm ctx
                    <null>
                    (env pool
                         [(r    (arg cr pool))
                          (a    (arg tr pool))
                          (b    (arg cb pool))
                          (v    (reg tr pool))
                          (w    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr (get-value r) (car (shape r))))
                         (CMP (get-value r) pend)
                         (JE 'return)
                         'loop
                         (MOV v a)
                         (if (eq? tb tr)
                           (SUB v (ptr tb (get-value b)))
                           (append
                             ((if (signed? tb) MOVSX MOVZX) w (ptr tb (get-value b)))
                             (SUB v w)))
                         (MOV (ptr tr (get-value r)) v)
                         (ADD (get-value r) (size-of tr))
                         (ADD (get-value b) (size-of tb))
                         (CMP pend (get-value r))
                         (JNE 'loop)
                         'return)
                    (flatten (map types (list cr ca cb)))))
         (proc (lambda (a b)
                 (let [(r (make cr #:size (size b)))]
                   (apply code (flatten (map content (list r a b))))
                   r)))]
    (add-method! - (make <method> #:specializers (list ca cb) #:procedure proc))
    (- a b)))
(define-method (- (a <sequence<>>) (b <sequence<>>))
  (let* [(ca   (class-of a))
         (cb   (class-of b))
         (cr   (coerce ca cb))
         (ta   (typecode ca))
         (tb   (typecode cb))
         (tr   (typecode cr))
         (pool (make <pool>))
         (code (asm ctx
                    <null>
                    (env pool
                         [(r    (arg cr pool))
                          (a    (arg ca pool))
                          (b    (arg cb pool))
                          (v    (reg tr pool))
                          (w    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr (get-value r) (car (shape r))))
                         (CMP (get-value r) pend)
                         (JE 'return)
                         'loop
                         ((if (eq? ta tr) MOV (if (signed? ta) MOVSX MOVZX)) v (ptr ta (get-value a)))
                         (if (eq? tb tr)
                           (SUB v (ptr tb (get-value b)))
                           (append
                             ((if (signed? tb) MOVSX MOVZX) w (ptr tb (get-value b)))
                             (SUB v w)))
                         (MOV (ptr tr (get-value r)) v)
                         (ADD (get-value r) (size-of tr))
                         (ADD (get-value a) (size-of ta))
                         (ADD (get-value b) (size-of tb))
                         (CMP pend (get-value r))
                         (JNE 'loop)
                         'return)
                    (flatten (map types (list cr ca cb)))))
         (proc (lambda (a b)
                 (let [(r (make cr #:size (size a)))]
                   (apply code (flatten (map content (list r a b))))
                   r)))]
    (add-method! - (make <method> #:specializers (list ca cb) #:procedure proc))
    (- a b)))

(define-method (fill (t <meta<element>>) (n <integer>) value)
  (let* [(cr   (sequence t))
         (pool (make <pool>))
         (code (asm ctx
                    <null>
                    (env pool
                         [(r     (arg cr pool))
                          (value (arg t pool))
                          (pend  (reg <long> pool))]
                         (LEA pend (ptr t (get-value r) (car (shape r))))
                         (CMP (get-value r) pend)
                         (JE 'return)
                         'loop
                         (MOV (ptr t (get-value r)) value)
                         (ADD (get-value r) (size-of t))
                         (CMP (get-value r) pend)
                         (JNE 'loop)
                         'return)
                    (append (types cr) (list t))))
         (proc (lambda (t n value)
                 (let [(r (make cr #:size n))]
                   (apply code (flatten (list (content r) value)))
                   r)))]
    (add-method! fill (make <method>
                            #:specializers (list (class-of t) <integer> (class-of value))
                            #:procedure proc))
    (fill t n value)))
