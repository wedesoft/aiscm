(use-modules (aiscm xorg) (aiscm core))
(define img (fill <bool> '(60 100) #f))
(set img '(0 . 3) '(0 . 3) (arr (#f #t #f) (#f #f #t) (#t #t #t)))
(set img '(4 . 7) '(50 . 53) (arr (#t #t #t) (#f #f #t) (#f #t #f)))
(set img '(27 . 30) 25 #t)
(show
  (lambda (dsp)
    (let [(neighbours (convolve (to-type <ubyte> img) (arr (1 1 1) (1 0 1) (1 1 1))))]
      (set! img (&& (ge neighbours (where img 2 3)) (le neighbours 3)))
      (where img 255 0)))
  #:fullscreen #t #:io IO-OPENGL)
