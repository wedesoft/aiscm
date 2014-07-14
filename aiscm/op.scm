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
                         [(pr   (arg <long> pool))
                          (nr   (arg <long> pool))
                          (pa   (arg <long> pool))
                          (na   (arg <long> pool))
                          (b    (arg tr pool))
                          (a    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr pr nr))
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
                         [(pr   (arg <long> pool))
                          (nr   (arg <long> pool))
                          (a    (arg tr pool))
                          (pb   (arg <long> pool))
                          (nb   (arg <long> pool))
                          (b    (reg tr pool))
                          (r    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr pr nr))
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
                         [(pr   (arg <long> pool))
                          (nr   (arg <long> pool))
                          (pa   (arg <long> pool))
                          (na   (arg <long> pool))
                          (pb   (arg <long> pool))
                          (nb   (arg <long> pool))
                          (a    (reg tr pool))
                          (b    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr pr nr))
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
                    (flatten (map types (list cr ca cb)))))
         (proc (lambda (a b)
                 (let [(r  (make cr #:size (size a)))]
                   (if (not (= (size a) (size b)))
                     (throw 'array-dimensions-different (size a) (size b)))
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
                         [(pr   (arg <long> pool))
                          (nr   (arg <long> pool))
                          (pa   (arg <long> pool))
                          (na   (arg <long> pool))
                          (a    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr pr nr))
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
                    (flatten (map types (list cr ca)))))
         (proc (lambda (a)
                 (let [(r  (make cr #:size (size a)))]
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
                         [(pr   (arg <long> pool))
                          (nr   (arg <long> pool))
                          (pa   (arg <long> pool))
                          (na   (arg <long> pool))
                          (b    (arg tr pool))
                          (a    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr pr nr))
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
                         [(pr   (arg <long> pool))
                          (nr   (arg <long> pool))
                          (a    (arg tr pool))
                          (pb   (arg <long> pool))
                          (nb   (arg <long> pool))
                          (b    (reg tr pool))
                          (r    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr pr nr))
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
                    (flatten (map types (list cr ca cb)))))
         (proc (lambda (a b)
                 (let [(r  (make cr #:size (size b)))]
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
                         [(pr   (arg <long> pool))
                          (nr   (arg <long> pool))
                          (pa   (arg <long> pool))
                          (na   (arg <long> pool))
                          (pb   (arg <long> pool))
                          (nb   (arg <long> pool))
                          (a    (reg tr pool))
                          (b    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr pr nr))
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
                    (flatten (map types (list cr ca cb)))))
         (proc (lambda (a b)
                 (let [(r  (make cr #:size (size a)))]
                   (if (not (= (size a) (size b)))
                     (throw 'array-dimensions-different (size a) (size b)))
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
                    (append (types cr) (list t))))
         (proc (lambda (t n value)
                 (let [(r (make cr #:size n))]
                   (apply code (flatten (list (content r) value)))
                   r)))]
    (add-method! fill (make <method>
                            #:specializers (list (class-of t) <integer> (class-of value))
                            #:procedure proc))
    (fill t n value)))
