(define-module (aiscm op)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (aiscm mem)
  #:use-module (aiscm jit)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm sequence)
  #:export (fill)
  #:re-export (+ -))
(define ctx (make <jit-context>))

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
                    ca cb))
         (proc (lambda (a b) (make cr #:value (code (get-value a) (get-value b)))))]
    (add-method! + (make <method>
                         #:specializers (list ca cb)
                         #:procedure proc))
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
                         [(pr   (arg <long> pool))
                          (pa   (arg <long> pool))
                          (b    (arg tr pool))
                          (n    (arg <long> pool))
                          (a    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr pr n))
                         (CMP pr pend)
                         (JE 'return)
                         'loop
                         ((if (eq? ta tr) MOV (if (signed? ta) MOVSX MOVZX)) a (ptr ta pa))
                         (ADD a b)
                         (MOV (ptr tr pr) a)
                         (ADD pr (size-of tr))
                         (ADD pa (size-of ta))
                         (CMP pend pr)
                         (JNE 'loop)
                         'return)
                    <long> <long> cb <long>))
         (proc (lambda (a b)
                 (let* [(n (size a))
                        (r (make cr #:size n))
                        (pr ((compose pointer-address get-memory get-value) r))
                        (pa ((compose pointer-address get-memory get-value) a))]
                   (code pr pa (get-value b) n)
                   r)))]
    (add-method! + (make <method>
                         #:specializers (list ca cb)
                         #:procedure proc))
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
                         [(pr   (arg <long> pool))
                          (a    (arg tr pool))
                          (pb   (arg <long> pool))
                          (n    (arg <long> pool))
                          (b    (reg tr pool))
                          (r    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr pr n))
                         (CMP pr pend)
                         (JE 'return)
                         'loop
                         (MOV r a)
                         ((if (eq? tb tr) MOV (if (signed? tb) MOVSX MOVZX)) b (ptr tb pb))
                         (ADD r b)
                         (MOV (ptr tr pr) r)
                         (ADD pr (size-of tr))
                         (ADD pb (size-of tb))
                         (CMP pend pr)
                         (JNE 'loop)
                         'return)
                    <long> ca <long> <long>))
         (proc (lambda (a b)
                 (let* [(n  (size b))
                        (r  (make cr #:size n))
                        (pr ((compose pointer-address get-memory get-value) r))
                        (pb ((compose pointer-address get-memory get-value) b))]
                   (code pr (get-value a) pb n)
                   r)))]
    (add-method! + (make <method>
                         #:specializers (list ca cb)
                         #:procedure proc))
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
                         [(pr   (arg <long> pool))
                          (pa   (arg <long> pool))
                          (pb   (arg <long> pool))
                          (na   (arg <long> pool))
                          (a    (reg tr pool))
                          (b    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr pr na))
                         (CMP pr pend)
                         (JE 'return)
                         'loop
                         ((if (eq? ta tr) MOV (if (signed? ta) MOVSX MOVZX)) a (ptr ta pa))
                         (if (eq? tb tr)
                           (ADD a (ptr tb pb))
                           (append
                             ((if (signed? tb) MOVSX MOVZX) b (ptr tb pb))
                             (ADD a b)))
                         (MOV (ptr tr pr) a)
                         (ADD pr (size-of tr))
                         (ADD pa (size-of ta))
                         (ADD pb (size-of tb))
                         (CMP pend pr)
                         (JNE 'loop)
                         'return)
                    <long> <long> <long> <long>))
         (proc (lambda (a b)
                 (let* [(na (size a))
                        (nb (size b))
                        (r  (make cr #:size na))
                        (pr ((compose pointer-address get-memory get-value) r))
                        (pa ((compose pointer-address get-memory get-value) a))
                        (pb ((compose pointer-address get-memory get-value) b))]
                   (if (not (= na nb)) (throw 'array-dimensions-different na nb))
                   (code pr pa pb na)
                   r)))]
    (add-method! + (make <method>
                         #:specializers (list ca cb)
                         #:procedure proc))
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
                    ca))
         (proc (lambda (a) (make cr #:value (code (get-value a)))))]
    (add-method! - (make <method>
                         #:specializers (list ca)
                         #:procedure proc))
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
                         [(pr   (arg <long> pool))
                          (pa   (arg <long> pool))
                          (n    (arg <long> pool))
                          (a    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr pr n))
                         (CMP pr pend)
                         (JE 'return)
                         'loop
                         (MOV a (ptr ta pa))
                         (NEG a)
                         (MOV (ptr tr pr) a)
                         (ADD pr (size-of ta))
                         (ADD pa (size-of tr))
                         (CMP pend pr)
                         (JNE 'loop)
                         'return)
                    <long> <long> <long>))
         (proc (lambda (a)
                 (let* [(n  (size a))
                        (r  (make cr #:size n))
                        (pr ((compose pointer-address get-memory get-value) r))
                        (pa ((compose pointer-address get-memory get-value) a))]
                   (code pr pa n)
                   r)))]
    (add-method! - (make <method>
                         #:specializers (list ca)
                         #:procedure proc))
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
                    ca cb))
         (proc (lambda (a b) (make cr #:value (code (get-value a) (get-value b)))))]
    (add-method! - (make <method>
                         #:specializers (list ca cb)
                         #:procedure proc))
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
                         [(pr   (arg <long> pool))
                          (pa   (arg <long> pool))
                          (b    (arg tr pool))
                          (n    (arg <long> pool))
                          (a    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr pr n))
                         (CMP pr pend)
                         (JE 'return)
                         'loop
                         ((if (eq? ta tr) MOV (if (signed? ta) MOVSX MOVZX)) a (ptr ta pa))
                         (SUB a b)
                         (MOV (ptr tr pr) a)
                         (ADD pr (size-of tr))
                         (ADD pa (size-of ta))
                         (CMP pend pr)
                         (JNE 'loop)
                         'return)
                    <long> <long> cb <long>))
         (proc (lambda (a b)
                 (let* [(n (size a))
                        (r (make cr #:size n))
                        (pr ((compose pointer-address get-memory get-value) r))
                        (pa ((compose pointer-address get-memory get-value) a))]
                   (code pr pa (get-value b) n)
                   r)))]
    (add-method! - (make <method>
                         #:specializers (list ca cb)
                         #:procedure proc))
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
                         [(pr   (arg <long> pool))
                          (a    (arg tr pool))
                          (pb   (arg <long> pool))
                          (n    (arg <long> pool))
                          (b    (reg tr pool))
                          (r    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr pr n))
                         (CMP pr pend)
                         (JE 'return)
                         'loop
                         (MOV r a)
                         ((if (eq? tb tr) MOV (if (signed? tb) MOVSX MOVZX)) b (ptr tb pb))
                         (SUB r b)
                         (MOV (ptr tr pr) r)
                         (ADD pr (size-of tr))
                         (ADD pb (size-of tb))
                         (CMP pend pr)
                         (JNE 'loop)
                         'return)
                    <long> ca <long> <long>))
         (proc (lambda (a b)
                 (let* [(n  (size b))
                        (r  (make cr #:size n))
                        (pr ((compose pointer-address get-memory get-value) r))
                        (pb ((compose pointer-address get-memory get-value) b))]
                   (code pr (get-value a) pb n)
                   r)))]
    (add-method! - (make <method>
                         #:specializers (list ca cb)
                         #:procedure proc))
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
                         [(pr   (arg <long> pool))
                          (pa   (arg <long> pool))
                          (pb   (arg <long> pool))
                          (na   (arg <long> pool))
                          (a    (reg tr pool))
                          (b    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr pr na))
                         (CMP pr pend)
                         (JE 'return)
                         'loop
                         ((if (eq? ta tr) MOV (if (signed? ta) MOVSX MOVZX)) a (ptr ta pa))
                         (if (eq? tb tr)
                           (SUB a (ptr tb pb))
                           (append
                             ((if (signed? tb) MOVSX MOVZX) b (ptr tb pb))
                             (SUB a b)))
                         (MOV (ptr tr pr) a)
                         (ADD pr (size-of tr))
                         (ADD pa (size-of ta))
                         (ADD pb (size-of tb))
                         (CMP pend pr)
                         (JNE 'loop)
                         'return)
                    <long> <long> <long> <long>))
         (proc (lambda (a b)
                 (let* [(na (size a))
                        (nb (size b))
                        (r  (make cr #:size na))
                        (pr ((compose pointer-address get-memory get-value) r))
                        (pa ((compose pointer-address get-memory get-value) a))
                        (pb ((compose pointer-address get-memory get-value) b))]
                   (if (not (= na nb)) (throw 'array-dimensions-different na nb))
                   (code pr pa pb na)
                   r)))]
    (add-method! - (make <method>
                         #:specializers (list ca cb)
                         #:procedure proc))
    (- a b)))

(define-method (fill (t <meta<element>>) (n <integer>) value)
  (let* [(pool (make <pool>))
         (code (asm ctx
                    <null>
                    (env pool
                         [(pr    (arg <long> pool))
                          (n     (arg <long> pool))
                          (value (arg t pool))
                          (pend  (reg <long> pool))]
                         (LEA pend (ptr t pr n))
                         (CMP pr pend)
                         (JE 'return)
                         'loop
                         (MOV (ptr t pr) value)
                         (ADD pr (size-of t))
                         (CMP pr pend)
                         (JNE 'loop)
                         'return)
                    <long> <long> t))
         (proc (lambda (t n value)
                 (let* [(r  (make (sequence t) #:size n))
                        (pr ((compose pointer-address get-memory get-value) r))]
                   (code pr n value)
                   r)))]
    (add-method! fill (make <method>
                            #:specializers (list (class-of t) <integer> (class-of value))
                            #:procedure proc))
    (fill t n value)))
