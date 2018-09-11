#!/usr/bin/guile \
--no-auto-compile -s
!#
(use-modules (aiscm core))
(exit (equal? (to-list (+ (arr 2 3 5) 1)) '(3 4 6)))
