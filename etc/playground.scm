(use-modules (srfi srfi-64))
(use-modules (oop goops) (aiscm element) (aiscm int) (aiscm asm) (aiscm command) (aiscm variable) (aiscm program) (aiscm compile) (aiscm util) (srfi srfi-1))


(define (replace-variables allocation cmd temporaries)
  "Substitute variables with registers and add spill code using temporary registers if necessary"
  (let [(substituted (substitute-variables cmd allocation))
        (spilled?    (lambda (var) (is-a? (assq-ref allocation var) <address>)))]
    (if (is-a? substituted <cmd>)
      (let* [(target    (car (filter spilled? (append (get-ptr-args cmd) (output cmd) (input cmd)))))
             (location  (assq-ref allocation target))
             (is-input  (memv target (input cmd)))
             (is-output (memv target (output cmd)))
             (temporary (car temporaries))
             (typed-tmp (to-type (typecode target) temporary))]
        (filter identity
          (append (list (and is-input  (MOV typed-tmp location)))
                  (replace-variables allocation (substitute-variables cmd (list (cons target temporary))) (cdr temporaries))
                  (list (and is-output (MOV location typed-tmp))))))
      (list (substitute-variables cmd allocation)))))


(test-begin "playground")
(test-begin "replace-variables")
  (let [(a (var <int>))
        (x (var <sint>))
        (l (var <long>))
        (p (var <long>))]
  (test-equal "put instruction into a list if there are no variables to replace"
    (list (MOV EAX 0)) (replace-variables '() (MOV EAX 0) '()))
  (test-equal "replace input variable with allocated register"
    (list (MOV ESI ECX)) (replace-variables (list (cons a RCX)) (MOV ESI a) '()))
  (test-equal "use intermediate register for spilled input value"
    (list (MOV RAX (ptr <long> RSP 16)) (PUSH RAX)) (replace-variables (list (cons l (ptr <long> RSP 16))) (PUSH l) (list RAX)))
  (test-equal "use correct type for intermediate register"
    (list (MOV EAX (ptr <int> RSP 16)) (PUSH EAX)) (replace-variables (list (cons a (ptr <long> RSP 16))) (PUSH a) (list RAX)))
  (test-equal "use intermediate register for spilled output value"
    (list (POP RAX) (MOV (ptr <long> RSP 16) RAX)) (replace-variables (list (cons l (ptr <long> RSP 16))) (POP l) (list RAX)))
  (test-equal "fetch pointer value from stack"
    (list (MOV RAX (ptr <long> RSP 32)) (MOV EDX (ptr <int> RAX 8)))
    (replace-variables (list (cons a RDX) (cons p (ptr <long> RSP 32))) (MOV a (ptr <int> p 8)) (list RAX)))
  (test-equal "do not use temporary variable when reading from register pointer"
    (list (MOV EDX (ptr <int> RCX 8))) (replace-variables (list (cons a RDX) (cons p RCX)) (MOV a (ptr <int> p 8)) (list RAX)))
  (test-equal "only use temporaries for spilled variables"
    (list (MOV EAX (ptr <int> RSP 8)) (MOV (ptr <int> RDI) EAX))
    (replace-variables (list (cons p RDI) (cons a (ptr <long> RSP 8))) (MOV (ptr <int> p) a) (list RAX RCX)))
  (test-equal "use two temporary variables to write spilled value to spilled address value"
    (list (MOV RAX (ptr <long> RSP 8)) (MOV ECX (ptr <int> RSP 16)) (MOV (ptr <int> RAX 8) ECX))
    (replace-variables (list (cons p (ptr <long> RSP 8)) (cons a (ptr <long> RSP 16)))
                       (MOV (ptr <int> p 8) a) (list RAX RCX))))
(test-end "replace-variables")
(test-end "playground")
