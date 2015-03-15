(define-test "static (vector?)" (expect-static-success
  (assert-true  (vector? #(1 2 3)))
  (assert-true  (vector? #()))
  (assert-false (vector? 4))))

(define-test "dynamic (vector?)" (expect-success
  (assert-true  (vector? (typeless-cell #(1 2 3))))
  (assert-true  (vector? (typeless-cell #())))
  (assert-false (vector? (typeless-cell 4)))))

(define-test "(make-vector)" (expect-success
  (assert-equal #() (make-vector 0))
  (assert-equal #() (make-vector 0 5.0))
  (assert-equal #(#!unit #!unit #!unit) (make-vector 3))
  (assert-equal #(5.0 5.0 5.0) (make-vector 3 5.0))))

(define-test "(make-vector) with a negative length fails" (expect-error range-error?
  (force-evaluation (make-vector -3))))

(define-test "(vector) " (expect-success
  (assert-equal #() (vector))
  (assert-equal #(a b c) (vector 'a 'b 'c))
  (assert-equal #(#(1)) (vector (vector 1)))))

(define-test "(list->vector) an empty vector" (expect-success
  (assert-equal #() (list->vector '()))
  (assert-equal #(a b c) (list->vector '(a b c)))))

(define-test "(list->vector) with non-list fails" (expect-error type-error?
  (list->vector 'a)))

(define-test "static (vector-length)" (expect-static-success
  (assert-equal 3 (vector-length #(1 2 3)))
  (assert-equal 0 (vector-length #()))
  (assert-equal 0 (vector-length (make-vector 0 #f)))
  (assert-equal 15 (vector-length (make-vector 15 #f)))))

(define-test "static (vector-ref)" (expect-static-success
  (assert-equal 'c (vector-ref #(a b c d e f) 2))))

(define-test "vector-ref can return procedure" (expect 5
  ((vector-ref (vector +) 0) 2 3)))

(define-test "vector-ref past end of vector fails" (expect-error range-error?
  (vector-ref #(a b c d e f) 7)))

(define-test "vector-ref with negative index fails" (expect-error range-error?
  (vector-ref #(a b c d e f) -1)))

(define-test "vector-ref with non-integer fails" (expect-compile-error type-error?
  (vector-ref #(a b c d e f) "4")))

(define-test "vector-set!" (expect #(1 1 2 1 1)
  ; Need to make a new vector because vector literals are immutable
  (define test-vector (make-vector 5 1))
  (vector-set! test-vector 2 2)
  test-vector))

(define-test "vector-set! on vector literal fails" (expect-error mutate-literal-error?
  ; Need to make a new vector because vector literals are immutable
  (vector-set! #(1 2 3 4 5) 2 2)))

(define-test "vector-set! past end of vector fails" (expect-error range-error?
  (define test-vector (make-vector 5 1))
  (vector-set! test-vector 5 2)))

(define-test "vector-set! with negative index fails" (expect-error range-error?
  (define test-vector (make-vector 5 1))
  (vector-set! test-vector -1 2)))

(define-test "(vector-append)" (expect-success
  (assert-equal #() (vector-append))
  (assert-equal #(a b c) (vector-append #(a b c)))
  (assert-equal #(1 2 3 4 5 6) (vector-append #(1 2) #(3 4) #(5 6)))
  (assert-equal #() (vector-append #() #() #()))))

(define-test "(vector-append) with non-vector fails" (expect-error type-error?
  (vector-append '(1 2) '(3 4))))

(define-test "(vector->list)" (expect-success
  (assert-equal '() (vector->list #()))
  (assert-equal '(dah dah didah) (vector->list #(dah dah didah)))
  (assert-equal '(dah didah) (vector->list '#(dah dah didah) 1))
  (assert-equal '(dah) (vector->list '#(dah dah didah) 1 2))
  (assert-equal '() (vector->list '#(dah dah didah) 0 0))
  (assert-equal '() (vector->list '#(dah dah didah) 3 3))
  (assert-equal '(#(a b) #(c d) #(e f)) (vector->list #(#(a b) #(c d) #(e f))))))

(define-test "(vector->list) with backwards slice fails" (expect-error range-error?
  (vector->list '#(dah dah didah) 2 1)))

(define-test "(vector->list) past end of vector fails" (expect-error range-error?
  (vector->list '#(dah dah didah) 0 4)))

(define-test "(vector->list) with negative start index fails" (expect-error range-error?
  (vector->list '#(dah dah didah) -1)))

(define-test "(vector-copy)" (expect-success
  (assert-equal #() (vector-copy #()))
  (assert-equal #(1 2 3) (vector-copy #(1 2 3)))
  (assert-equal #(2 3) (vector-copy #(1 2 3) 1))
  (assert-equal #(2) (vector-copy #(1 2 3) 1 2))
  (assert-equal #() (vector-copy #(1 2 3) 0 0))
  (assert-equal #() (vector-copy #(1 2 3) 3 3))

  (define a #(1 8 2 8)) ; a may be immutable
  (define b (vector-copy a))
  (vector-set! b 0 3) ; b is mutable
  (assert-equal #(3 8 2 8) b)
  (define c (vector-copy b 1 3))
  (assert-equal #(8 2) c)))

(define-test "(vector-copy) with backwards slice fails" (expect-error range-error?
  (vector-copy '#(dah dah didah) 2 1)))

(define-test "(vector-copy) past end of vector fails" (expect-error range-error?
  (vector-copy '#(dah dah didah) 0 4)))

(define-test "(vector-copy) with negative start index fails" (expect-error range-error?
  (vector-copy '#(dah dah didah) -1)))

(define-test "(vector-fill!)" (expect-success
  (define a (vector 1 2 3 4 5))
  (vector-fill! a 'smash 2 4)

  (assert-equal #(1 2 smash smash 5) a)

  (vector-fill! a 'bang 4)
  (assert-equal #(1 2 smash smash bang) a)

  (vector-fill! a 'empty 0 0)
  (vector-fill! a 'empty 5 5)
  (assert-equal #(1 2 smash smash bang) a)

  (vector-fill! a 'all)
  (assert-equal #(all all all all all) a)))

(define-test "(vector-fill!) on vector literal fails" (expect-error mutate-literal-error?
  (vector-fill! #(dah dah didah) #t)))

(define-test "(vector-fill!) with backwards slice fails" (expect-error range-error?
  (vector-fill! (vector 'dah 'dah 'didah) #t 2 1)))

(define-test "(vector-fill!) past end of vector fails" (expect-error range-error?
  (vector-fill! (vector 'dah 'dah 'didah) #t 0 4)))

(define-test "(vector-fill!) with negative start index fails" (expect-error range-error?
  (vector-fill! (vector 'dah 'dah 'didah) #t -1)))

(define-test "(string->vector)" (expect-success
  (assert-equal #(#\A #\B #\C) (string->vector "ABC"))
  (assert-equal #(#\H #\e #\l #\l #\x2603 #\!) (string->vector "Hell☃!"))
  (assert-equal #(#\l #\l #\x2603 #\!) (string->vector "Hell☃!" 2))
  (assert-equal #(#\l #\l) (string->vector "Hell☃!" 2 4))
  (assert-equal #() (string->vector "Hell☃!" 0 0))
  (assert-equal #() (string->vector "Hell☃!" 6 6))))

(define-test "(string->vector) with backwards slice fails" (expect-error range-error?
  (string->vector "Hell☃!" 2 1)))

(define-test "(string->vector) past end of string fails" (expect-error range-error?
  (string->vector "Hell☃!" 0 8)))

(define-test "(string->vector) with negative start index fails" (expect-error range-error?
  (string->vector "Hell☃!" -1)))

(define-test "(vector->string)" (expect-success
  (assert-equal "ABC" (vector->string #(#\A #\B #\C)))
  (assert-equal "Hell☃!" (vector->string #(#\H #\e #\l #\l #\x2603 #\!)))
  (assert-equal "ll☃!" (vector->string #(#\H #\e #\l #\l #\x2603 #\!) 2))
  (assert-equal "ll" (vector->string #(#\H #\e #\l #\l #\x2603 #\!) 2 4))
  (assert-equal "" (vector->string #(#\H #\e #\l #\l #\x2603 #\!) 0 0))
  (assert-equal "" (vector->string #(#\H #\e #\l #\l #\x2603 #\!) 6 6))))

(define-test "(vector->string) with constant non-char fails" (expect-error type-error?
  (vector->string #(#\H #\e #\l #\l 'notchar #\!))))

(define-test "(vector->string) with dynamic non-char fails" (expect-error type-error?
  (define test-vector (vector #\H #\e #\l #\l 'notchar #\!))
  (vector->string test-vector)))

(define-test "(vector->string) with backwards slice fails" (expect-error range-error?
  (vector->string #(#\H #\e #\l #\l #\x2603 #\!) 2 1)))

(define-test "(vector->string) past end of string fails" (expect-error range-error?
  (vector->string #(#\H #\e #\l #\l #\x2603 #\!) 0 8)))

(define-test "(vector->string) with negative start index fails" (expect-error range-error?
  (vector->string #(#\H #\e #\l #\l #\x2603 #\!) -1)))

(define-test "(vector-copy!)" (expect-success
  (define a (vector 1 2 3 4 5))
  (define b (vector 10 20 30 40 50))
  (vector-copy! b 1 a 0 2)

  (assert-equal #(10 1 2 40 50) b)

  (vector-copy! b 1 a 0 0)
  (vector-copy! b 1 a 5)
  (assert-equal #(10 1 2 40 50) b)

  (vector-copy! b 0 a)
  (assert-equal #(1 2 3 4 5) b)))

(define-test "(vector-copy!) on vector literal fails" (expect-error mutate-literal-error?
  (define a (vector 1 2 3 4 5))
  (define b #(10 20 30 40 50))
  (vector-copy! b 1 a 0 2)))

(define-test "(vector-copy!) with backwards slice fails" (expect-error range-error?
  (define a (vector 1 2 3 4 5))
  (define b (vector 10 20 30 40 50))
  (vector-copy! b 1 a 2 0)))

(define-test "(vector-copy!) past end of from fails" (expect-error range-error?
  (define a (vector 1 2 3 4 5))
  (define b (vector 10 20 30 40 50))
  (vector-copy! b 2 a 4 6)))

(define-test "(vector-copy!) past end of to fails" (expect-error range-error?
  (define a (vector 1 2 3 4 5))
  (define b (vector 10 20 30 40 50))
  (vector-copy! b 2 a 1)))

(define-test "(make-vector) of large vector" (expect-success
  ; Make sure the planner can deal with large vectors
  (define vec1 (make-vector 10000000))
  (define vec2 (make-vector 10000000))
  (assert-true (equal? vec1 vec2))))
