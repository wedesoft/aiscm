(use-modules (aiscm core))
(convolve (arr (0 0 0 0 0) (0 0 1 0 0) (0 0 0 0 0)) (arr (-1 0 1) (-2 0 2) (-1 0 1)))
;#<sequence<sequence<int<16,signed>>>>:
;((0 -1 0 1 0)
; (0 -2 0 2 0)
; (0 -1 0 1 0))
