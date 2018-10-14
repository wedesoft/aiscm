(use-modules (oop goops) (aiscm util) (aiscm tensors ) (system foreign) (rnrs bytevectors) (aiscm core) (aiscm image) (aiscm magick) (aiscm xorg) (aiscm v4l2) (srfi srfi-1) (srfi srfi-26))

(define a (arr 2 3 5))
(define-tensor (trivial x) x)

(trivial a)

(define (evaluate-tensor expression)
    (let [(result (allocate-array (typecode expression) (apply llvmlist (shape expression))))]; TODO: have shape return <llvmlist>
      result))

(define (adapted-native-type value) (if (is-a? value <integer>) <int> (native-type value)))

(define (trivial2 x)
    (let [(fun (jit (map adapted-native-type (list x))
                 (lambda arguments (apply (lambda (x) (evaluate-tensor x)) (map expression->tensor arguments)))))]
      (fun x)))

(trivial2 a)
