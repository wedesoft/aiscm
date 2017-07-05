(use-modules (srfi srfi-64))
(use-modules (oop goops) (aiscm element) (aiscm int) (aiscm asm) (aiscm command) (aiscm variable) (aiscm program) (aiscm compile) (aiscm util) (srfi srfi-1))

(define (replace-variables allocation cmd temporaries)
  "Substitute variables with registers and add spill code using temporary registers if necessary"
  (let* [(primary-var (first-argument cmd))
         (primary-loc (assq-ref allocation primary-var))]
    (if (is-a? primary-loc <address>)
      (let* [(register   (car temporaries))
             (temporary  (to-type (typecode primary-var) register))
             (is-input?  (memv primary-var (input cmd)))
             (is-output? (memv primary-var (output cmd)))]
        (filter identity
          (cons (and is-input? (MOV temporary primary-loc))
                (attach (replace-variables allocation
                                           (substitute-variables cmd (list (cons primary-var register)))
                                           (cdr temporaries))
                        (and is-output? (MOV primary-loc temporary))))))
      (let [(spilled-pointers (filter (lambda (arg) (is-a? (assq-ref allocation arg) <address>)) (get-ptr-args cmd)))]
        (attach (map (lambda (var temporary) (MOV temporary (assq-ref allocation var))) spilled-pointers temporaries)
                (substitute-variables cmd (fold (lambda (var tmp alist) (assq-set alist var tmp))
                                                allocation spilled-pointers temporaries)))))))

(test-begin "playground")
(test-begin "replace-variables")
  (let [(a (var <int>))
        (x (var <sint>))
        (p (var <long>))]
  (test-equal "put instruction into a list if there are no variables to replace"
    (list (MOV EAX 0)) (replace-variables '() (MOV EAX 0) '()))
  (test-equal "replace input variable with allocated register"
    (list (MOV ESI ECX)) (replace-variables (list (cons a RCX)) (MOV ESI a) '()))
  (test-equal "replace output variable with allocated register"
    (list (MOV ECX 0)) (replace-variables (list (cons a RCX)) (MOV a 0) '()))
  (test-equal "read input variable from spill location"
    (list (MOV EDX (ptr <int> RSP 16))) (replace-variables (list (cons a (ptr <long> RSP 16))) (MOV EDX a) '()))
  (test-equal "use temporary register for first argument and fetch value from spill location"
    (list (MOV AX (ptr <sint> RSP 16)) (CMP AX 0)) (replace-variables (list (cons x (ptr <long> RSP 16))) (CMP x 0) (list RAX)))
  (test-equal "use correct type for temporary register"
    (list (MOV EAX (ptr <int> RSP 16)) (CMP EAX 0)) (replace-variables (list (cons a (ptr <long> RSP 16))) (CMP a 0) (list RAX)))
  (test-equal "read and write back argument from stack into temporary register"
    (list (MOV EAX (ptr <int> RSP 16)) (ADD EAX 1) (MOV (ptr <int> RSP 16) EAX))
    (replace-variables (list (cons a (ptr <long> RSP 16))) (ADD a 1) (list RAX)))
  (test-equal "write output value in temporary register to the stack"
    (list (MOV EAX 1) (MOV (ptr <int> RSP 16) EAX)) (replace-variables (list (cons a (ptr <long> RSP 16))) (MOV a 1) (list RAX)))
  (test-equal "use temporary variable to implement reading from pointer to pointer"
    (list (MOV RAX (ptr <long> RSP 32)) (MOV EDX (ptr <int> RAX 8)))
    (replace-variables (list (cons a RDX) (cons p (ptr <long> RSP 32))) (MOV a (ptr <int> p 8)) (list RAX)))
  (test-equal "do not use temporary variable when reading from register pointer"
    (list (MOV EDX (ptr <int> RCX 8))) (replace-variables (list (cons a RDX) (cons p RCX)) (MOV a (ptr <int> p 8)) (list RAX)))
  (test-equal "use temporary variable to implement writing to pointer to pointer"
    (list (MOV RAX (ptr <long> RSP 32)) (MOV (ptr <int> RAX 8) EDX))
    (replace-variables (list (cons a RDX) (cons p (ptr <long> RSP 32))) (MOV (ptr <int> p 8) a) (list RAX)))
  (test-equal "use two temporary variables to write spilled value to spilled address value"
    (list (MOV RAX (ptr <long> RSP 8)) (MOV ECX (ptr <int> RSP 16)) (MOV (ptr <int> RAX 8) ECX))
    (replace-variables (list (cons p (ptr <long> RSP 8)) (cons a (ptr <long> RSP 16)))
                       (list (MOV (ptr <int> p 8) a)) (list RAX RCX))))
(test-end "replace-variables")
(test-end "playground")
