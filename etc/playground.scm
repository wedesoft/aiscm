(use-modules (srfi srfi-64))
(use-modules (srfi srfi-26))
(use-modules (srfi srfi-1))
(use-modules (oop goops) (aiscm asm) (aiscm jit) (aiscm element) (aiscm int) (aiscm sequence) (aiscm pointer) (aiscm expression) (aiscm operation) (aiscm util) (aiscm program) (aiscm register-allocate) (aiscm compile) (aiscm live-analysis) (aiscm variable) (aiscm command) (aiscm bool))

(to-type <byte> (seq <int> 2 3 5))

(define-method (bits3 (x <xmm>)) (bits3 (get-code x)))


(define (VEX xmm)
  (list #xc5 (logior #x82 (ash (logxor #xf (get-code xmm)) 3))))

(define (VCVTSI2SS xmm ignore reg)
  (append (VEX xmm) (list #x2a) (ModR/M 3 xmm reg)))

(define ctx (make <context>))
(define target <byte>)
(define self (seq <int> 2 3 5))
;(jit ctx (list (class-of self)) (cut to-type target <>))

(define context ctx)
(define classes (list (class-of self)))
(define proc (cut to-type target <>))
(define args         (map skeleton classes))
(define parameters   (map parameter args))
(define expr         (apply proc parameters))
(define result-type  (type expr))
(define intermediate (parameter result-type))
(define result       (generate-return-code args intermediate expr))
(define lst          (apply assemble result))
(define results      (list-ref lst 0))
(define parameters   (list-ref lst 1))
(define instructions (list-ref lst 2))
(define registers    default-registers)
(define blocked (blocked-intervals instructions))
(define prog(flatten-code (relabel (filter-blocks instructions))))

((asm ctx <int> (list <int>) (list (list #xc5 #xfa #x2a #xc7) (list #xc5 #xfa #x2c #xc0) (RET))) 42)

; TODO: compiled-copy -> set3
; TODO: set array using list
; MOVD

; VEX.NDS.LIG.F3.0F.W0 2A/r

(test-begin "playground")
(test-equal "Set XMM0 to EDI's integer value"
  '(#xc5 #xfa #x2a #xc7) (VCVTSI2SS XMM0 XMM0 EDI))
(test-equal "Set XMM1 to EDI's integer value"
  '(#xc5 #xf2 #x2a #xcf) (VCVTSI2SS XMM1 XMM1 EDI))
(test-equal "Set XMM0 to ESI's integer value"
  '(#xc5 #xfa #x2a #xc6) (VCVTSI2SS XMM0 XMM0 ESI))

(test-end "playground")
