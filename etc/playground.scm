(use-modules (oop goops)
             (srfi srfi-1)
             (ice-9 curried-definitions)
             (aiscm operation)
             (aiscm expression)
             (aiscm asm)
             (aiscm element)
             (aiscm scalar)
             (aiscm int)
             (aiscm sequence)
             (aiscm rgb)
             (aiscm jit)
             (aiscm util))

(test-begin "playground")

(define-method (delegate-op (target <meta<scalar>>) (intermediate <meta<scalar>>) name out args)
  ((apply name (map type args)) out args))
(define-method (delegate-op (target <meta<sequence<>>>) (intermediate <meta<sequence<>>>) name out args)
  ((apply name (map type args)) out args))
(define-method (delegate-op target intermediate name out args)
  (let [(result (apply name (map (lambda (arg) (decompose-value (type arg) arg)) args)))]
    (if (is-a? result <list>); TODO: fix this hack
      result
      (append-map code (content (type out) out) (content (type result) result)))))
(define ((delegate-fun name) out args)
  (delegate-op (type out) (reduce coerce #f (map type args)) name out args))

(define ((cumulative-code op) out args)
  "Adapter for cumulative operations"
  (operation-code (type out) op (car args) (cdr args)))

(define a (parameter <int>))
(define b (parameter <int>))

(define-method (+= (a <param>) (b <param>)) ((delegate-fun +=) a (list a b)))
(define-method (+= (a <meta<int<>>>) (b <meta<int<>>>)) (cumulative-code ADD))

(+= a b)

(define c (parameter <intrgb>))
(define d (parameter <intrgb>))

(define-method (+= (a <rgb>) (b <rgb>))
  (append-map += (list (red a) (green a) (blue a)) (list (red b) (green b) (blue b))))

(+= c d)

(define-method (+= (a <rgb>) (b <rgb>))
  (append-map += (list (red a) (green a) (blue a)) (list (red b) (green b) (blue b))))

(+= (decompose-value <intrgb> c) (decompose-value <intrgb> d))

(test-end "playground")
