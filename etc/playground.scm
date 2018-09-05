(use-modules (oop goops) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm core) (aiscm image) (aiscm magick) (aiscm xorg) (aiscm v4l2) (srfi srfi-1) (srfi srfi-26))

(
  (jit (list <int>)
    (lambda (n)
      (let [(start (make-basic-block "start"))
            (for   (make-basic-block "for"))
            (body  (make-basic-block "body"))
            (end   (make-basic-block "end"))
            ]
        (llvm-begin
          (build-branch start)
          (position-builder-at-end start)
          (jit-let [(i (typed-constant <int> 0))
                    (s (typed-constant <int> 0))]
            (build-branch for)
            (position-builder-at-end for)
            (jit-let [(i1 (build-phi (class-of i)))
                      (s1 (build-phi (class-of s)))]
              (add-incoming i1 start i)
              (add-incoming s1 start s)
              (build-cond-branch (lt i1 n) body end)
              (position-builder-at-end body)
              (jit-let [(i2 (+ i1 1))
                        (s2 (+ s1 i1))]
                (add-incoming i1 body i2)
                (add-incoming s1 body s2)
                (build-branch for))
              (position-builder-at-end end)
              s1))))))
 10)
