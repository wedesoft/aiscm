(use-modules (oop goops)
             (srfi srfi-1)
             (ice-9 curried-definitions)
             (aiscm operation)
             (aiscm expression)
             (aiscm asm)
             (aiscm element)
             (aiscm int)
             (aiscm rgb)
             (aiscm jit)
             (aiscm util))

(test-begin "playground")

(define ctx (make <context>))

(define a (parameter <int>))
(define b (parameter <int>))

(+= a b)

(define c (parameter <intrgb>))
(define d (parameter <intrgb>))

(+= c d)

(define-method (+= (a <rgb>) (b <rgb>))
  (append-map += (list (red a) (green a) (blue a)) (list (red b) (green b) (blue b))))

(+= (decompose-value <intrgb> c) (decompose-value <intrgb> d))

(define-method (+= (a <meta<int<>>>)) (cumulative-code ADD))

(define-method (+= (a <param>) (b <param>)) ((delegate-fun +=) a (list b)))

(test-end "playground")
