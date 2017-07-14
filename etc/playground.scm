(use-modules (srfi srfi-64))
(use-modules (oop goops) (srfi srfi-1) (srfi srfi-26) (aiscm asm) (aiscm int) (aiscm rgb) (aiscm expression) (aiscm jit) (aiscm operation) (aiscm element) (aiscm sequence) (aiscm scalar) (ice-9 curried-definitions) (aiscm composite) (aiscm tensor))

; TODO: intermediates

(define ((delegate-plus-fun name) out . args) (apply (apply name (map type args)) out args))
(define-method (+ (a <param>) (b <param>)) (make-function + coerce (delegate-plus-fun +) (list a b)))
(define-method (+= (a <param>) (b <param>)) ((delegate-plus-fun +=) a a b))

(define-method (+ (a <meta<composite>>) (b <meta<element>>))
  (lambda (out . args)
    (let [(result (apply + (map (lambda (arg) (decompose-value (type arg) arg)) args)))]
      (append-map duplicate (content (type out) out) (content (type result) result)))))
(define-method (+ (a <meta<element>>) (b <meta<composite>>))
  (lambda (out . args)
    (let [(result (apply + (map (lambda (arg) (decompose-value (type arg) arg)) args)))]
      (append-map duplicate (content (type out) out) (content (type result) result)))))
(define-method (+= (ta <meta<rgb<>>>) (tb <meta<rgb<>>>))
  (lambda (r a b) (append-map (+= (base ta) (base tb)) (content <rgb<>> r) (content <rgb<>> a) (content <rgb<>> b))))
(define-method (+= (ta <meta<rgb<>>>) (tb <meta<element>>))
  (lambda (r a b) (append-map (+= (base ta) tb) (content <rgb<>> r) (content <rgb<>> a) (list b b b))))


(define r (parameter <int>))
(define a (parameter <int>))
(define b (parameter <int>))

((cumulative-code ADD) r r b)
((mutating-code +=) r a b)

((+= <int> <int>) r r b)
((+ <int> <int>) r a b)

(define r (parameter (sequence <int>)))
(define s (parameter (sequence <int>)))
(define c (parameter <int>))

(duplicate r (+ s c))

(test-begin "playground")
(test-end "playground")
