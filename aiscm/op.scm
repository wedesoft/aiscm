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
(define-method (fill (t <meta<element>>) (n <integer>) value)
  (let* [(pool (make <pool>))
         (code (asm ctx
                    <null>
                    (env pool
                         [(pr    (arg <long> pool))
                          (n     (arg <long> pool))
                          (value (arg t pool))
                          (pend  (reg <long> pool))]
                         (LEA pend (ptr t pr n (scale t)))
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
                        (pr ((compose pointer-address get-memory get-value get-value) r))]
                   (code pr n value)
                   r)))]
    (add-method! fill (make <method>
                            #:specializers (list (class-of t) <integer> (class-of value))
                            #:procedure proc))
    (fill t n value)))
(define-method (+ (a <element>)) a)
(define-method (+ (a <element>) (b <element>))
  (let* [(ta     (class-of a))
         (tb     (class-of b))
         (tr     (coerce ta tb))
         (pool   (make <pool>))
         (code   (asm ctx
                      tr
                      (env pool
                           [(a (arg tr pool))
                            (b (arg tr pool))
                            (r (reg tr pool))]
                           (MOV r a)
                           (ADD r b))
                      ta tb))
         (proc   (lambda (a b) (make tr #:value (code (get-value a) (get-value b)))))]
    (add-method! + (make <method>
                         #:specializers (list ta tb)
                         #:procedure proc))
    (+ a b)))
(define-method (+ (a <element>) (b <integer>))
  (+ a (make (match b) #:value b)))
(define-method (+ (a <integer>) (b <element>))
  (+ (make (match a) #:value a) b))
(define-method (+ (a <sequence<>>) (b <element>))
  (let* [(ta   (typecode (class-of a)))
         (tb   (class-of b))
         (tr   (coerce ta tb))
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
                         (LEA pend (ptr tr pr n (scale tr)))
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
                    <long> <long> tb <long>))
         (proc (lambda (a b)
                 (let* [(n (get-size a))
                        (r (make (sequence tr) #:size n))
                        (pr ((compose pointer-address get-memory get-value get-value) r))
                        (pa ((compose pointer-address get-memory get-value get-value) a))]
                   (code pr pa (get-value b) n)
                   r)))]
    (add-method! + (make <method>
                         #:specializers (list (sequence ta) tb)
                         #:procedure proc))
    (+ a b)))
(define-method (+ (a <element>) (b <sequence<>>))
  (let* [(ta   (class-of a))
         (tb   (typecode (class-of b)))
         (tr   (coerce ta tb))
         (pool (make <pool>))
         (code (asm ctx
                    <null>
                    (env pool
                         [(pr   (arg <long> pool))
                          (a    (arg tr pool))
                          (pb   (arg <long> pool))
                          (n    (arg <long> pool))
                          (b    (reg tr pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr tr pr n (scale tr)))
                         (CMP pr pend)
                         (JE 'return)
                         'loop
                         ((if (eq? tb tr) MOV (if (signed? tb) MOVSX MOVZX)) b (ptr tb pb))
                         (ADD b a)
                         (MOV (ptr tr pr) b)
                         (ADD pr (size-of tr))
                         (ADD pb (size-of tb))
                         (CMP pend pr)
                         (JNE 'loop)
                         'return)
                    <long> ta <long> <long>))
         (proc (lambda (a b)
                 (let* [(n  (get-size b))
                        (r  (make (sequence tr) #:size n))
                        (pr ((compose pointer-address get-memory get-value get-value) r))
                        (pb ((compose pointer-address get-memory get-value get-value) b))]
                   (code pr (get-value a) pb n)
                   r)))]
    (add-method! + (make <method>
                         #:specializers (list ta (sequence tb))
                         #:procedure proc))
    (+ a b)))
(define-method (+ (a <sequence<>>) (b <sequence<>>))
  (let* [(ta   (typecode (class-of a)))
         (tb   (typecode (class-of b)))
         (tr   (coerce ta tb))
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
                         (LEA pend (ptr tr pr na (scale tr)))
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
                 (let* [(na (get-size a))
                        (nb (get-size b))
                        (r  (make (sequence tr) #:size na))
                        (pr ((compose pointer-address get-memory get-value get-value) r))
                        (pa ((compose pointer-address get-memory get-value get-value) a))
                        (pb ((compose pointer-address get-memory get-value get-value) b))]
                   (if (not (= na nb)) (throw 'array-dimensions-different na nb))
                   (code pr pa pb na)
                   r)))]
    (add-method! + (make <method>
                         #:specializers (list (sequence ta) (sequence tb))
                         #:procedure proc))
    (+ a b)))
(define-method (- (a <element>))
  (let* [(t    (class-of a))
         (pool (make <pool>))
         (code (asm ctx
                    t
                    (env pool
                         [(a (arg t pool))
                          (r (reg t pool))]
                         (MOV r a)
                         (NEG r))
                    t))
         (proc (lambda (a) (make t #:value (code (get-value a)))))]
    (add-method! - (make <method>
                         #:specializers (list t)
                         #:procedure proc))
    (- a)))
(define-method (- (a <sequence<>>))
  (let* [(t    (typecode (class-of a)))
         (pool (make <pool>))
         (code (asm ctx
                    <null>
                    (env pool
                         [(pr   (arg <long> pool))
                          (pa   (arg <long> pool))
                          (n    (arg <long> pool))
                          (a    (reg t pool))
                          (pend (reg <long> pool))]
                         (LEA pend (ptr t pr n (scale t)))
                         (CMP pr pend)
                         (JE 'return)
                         'loop
                         (MOV a (ptr t pa))
                         (NEG a)
                         (MOV (ptr t pr) a)
                         (ADD pr (size-of t))
                         (ADD pa (size-of t))
                         (CMP pend pr)
                         (JNE 'loop)
                         'return)
                    <long> <long> <long>))
         (proc (lambda (a)
                 (let* [(n  (get-size a))
                        (r  (make (sequence t) #:size n))
                        (pr ((compose pointer-address get-memory get-value get-value) r))
                        (pa ((compose pointer-address get-memory get-value get-value) a))]
                   (code pr pa n)
                   r)))]
    (add-method! - (make <method>
                         #:specializers (list (sequence t))
                         #:procedure proc))
    (- a)))
(define-method (- (a <element>) (b <element>))
  (let* [(ta   (class-of a))
         (tb   (class-of b))
         (tr   (coerce ta tb))
         (pool (make <pool>))
         (code (asm ctx
                    tr
                    (env pool
                         [(a (arg tr pool))
                          (b (arg tr pool))
                          (r (reg tr pool))]
                         (MOV r a)
                         (SUB r b))
                    ta tb))
         (proc (lambda (a b) (make tr #:value (code (get-value a) (get-value b)))))]
    (add-method! - (make <method>
                         #:specializers (list ta tb)
                         #:procedure proc))
    (- a b)))
(define-method (- (a <element>) (b <integer>))
  (- a (make (match b) #:value b)))
(define-method (- (a <integer>) (b <element>))
  (- (make (match a) #:value a) b))
