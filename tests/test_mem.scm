(use-modules
  (oop goops)
  (aiscm mem)
  (ice-9 regex)
  (ggspec lib))

(suite "The <mem> object"
  (tests
    (test
      "base pointer protects allocated memory from garbage collector"
      e
      (assert-equal (get-memory (e 'm)) (slot-ref (e 'm) 'base)))
    (test "'get-size' returns size of allocated memory"
      e
      (assert-equal 10 (get-size (e 'm))))
    (test "base pointer is copied when creating pointer with offset"
      e
      (assert-equal (get-memory (e 'm)) (slot-ref (+ (e 'm) 6) 'base)))
    (test "base pointer is copied when creating pointer with offset"
      e
      (assert-equal (get-memory (e 'm)) (slot-ref (+ (e 'm) 6) 'base)))
    (test "equal mem objects"
      e
      (assert-equal (+ (e 'm) 1) (+ (e 'm) 1)))
    (test "unequal mem objects"
      e
      (assert-not-equal
        (+ (e 'm) 1)
        (+ (e 'm) 2)))
    (test "pointer operations keep track of memory size"
      e
      (assert-equal 4 (get-size (+ (e 'm) 6))))
    (test "throw exception when pointer offset is negative"
      e
      (assert-true (error? (+(e 'm) -1))))
    (test "throw exception when pointer offset exceeds memory boundary"
      e
      (assert-true (error? (+(e 'm) 11))))
    (test "writing and reading to/from memory"
      e
      (assert-equal
        #vu8(2 3 5)
        (begin
          (write-bytes (e 'm) #vu8(2 3 5 7))
          (read-bytes (e 'm) 3))))
    (test "writing and reading with offset to/from memory"
      e
      (assert-equal
        #vu8(3 5 7)
        (begin
          (write-bytes (e 'm) #vu8(2 3 5 7))
          (read-bytes (+ (e 'm) 1) 3))))
    (test "writing with overlap and reading back"
      e
      (assert-equal
        #vu8(2 2 1 1)
        (begin
          (write-bytes (e 'm) #vu8(1 1 1 1))
          (write-bytes (e 'm) #vu8(2 2))
          (read-bytes (e 'm) 4))))
    (test "throw exception when reading past memory boundary"
      e
      (assert-true (error? (read-bytes (e 'm) 11))))
    (test
      "throw exception when attempting to write past memory boundary"
      e
      (assert-true
        (error?
          (write-bytes (e 'm) #vu8(1 2 3 4 5 6 7 8 9 10 11)))))
    (test "display mem object"
      e
      (assert-true
        (string-match
          "^#<<mem> #x[0-9a-f]* 10>$"
          (call-with-output-string
            (lambda (port) (display (e 'm) port))))))
    (test "write mem object"
      e
      (assert-true
        (string-match
          "^#<<mem> #x[0-9a-f]* 10>$"
          (call-with-output-string
            (lambda (port) (write (e 'm) port)))))))
  (options
    (option 'output-cb output-tap)
    (option 'tally #t))
  (setups
    (setup 'm
      (make <mem> #:size 10))))

