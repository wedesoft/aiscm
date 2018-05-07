#!/usr/bin/guile \
--no-auto-compile -s
!#
(use-modules (aiscm sequence) (aiscm int) (aiscm jit))
(exit (equal? (to-list (+ (seq 2 3 5) 1)) '(3 4 6)))
