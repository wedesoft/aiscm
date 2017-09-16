(use-modules (srfi srfi-64))
(use-modules (srfi srfi-26))
(use-modules (srfi srfi-1))
(use-modules (oop goops) (aiscm asm) (aiscm jit) (aiscm element) (aiscm int) (aiscm sequence) (aiscm pointer) (aiscm expression) (aiscm operation) (aiscm util) (aiscm program) (aiscm register-allocate) (aiscm compile) (aiscm live-analysis) (aiscm variable) (aiscm command) (aiscm bool))

(+ (seq <uint> 1 2 3) (seq <int> 0 0 0))

(test-begin "playground")
(test-end "playground")
