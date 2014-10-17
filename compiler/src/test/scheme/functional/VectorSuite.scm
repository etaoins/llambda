(define-test "(vector?)" (expect-success
	(assert-true  (vector? #(1 2 3)))
	(assert-true  (vector? (typeless-cell #(1 2 3))))

	(assert-true  (vector? #()))
	(assert-true  (vector? (typeless-cell #())))

	(assert-false (vector? 4))
	(assert-false (vector? (typeless-cell 4)))))

(define-test "(make-vector) an uninitialized empty vector" (expect #()
	(make-vector 0)))

(define-test "(make-vector) an filled empty vector" (expect #()
	(make-vector 0 5.0)))

(define-test "(make-vector) an unintialized non-empty vector" (expect #(#!unit #!unit #!unit)
	(make-vector 3)))

(define-test "(make-vector) a filled non-empty vector" (expect #(5.0 5.0 5.0)
	(make-vector 3 5.0)))

(define-test "(vector) an empty vector" (expect #()
	(vector)))

(define-test "(vector) a non-empty vector" (expect #(a b c)
	(vector 'a 'b 'c)))

(define-test "(list->vector) an empty vector" (expect #()
	(list->vector '())))

(define-test "(list->vector) a non-empty vector" (expect #(a b c)
	(list->vector '(a b c))))

(define-test "(list->vector) with non-list fails" (expect-failure
	(list->vector 'a)))

(define-test "vector length of non-empty constant vector" (expect 3
	(vector-length #(1 2 3))))

(define-test "vector length of empty constant vector" (expect 0
	(vector-length #())))

(define-test "vector length of non-empty constructed vector" (expect 15
	(vector-length (make-vector 15 #f))))

(define-test "vector length of empty constructed vector" (expect 0
	(vector-length (make-vector 0 #f))))

(define-test "vector-ref" (expect c
	(vector-ref #(a b c d e f) 2)))

(define-test "vector-ref can return procedure" (expect 5
	((vector-ref (vector +) 0) 2 3)))

(define-test "vector-ref out of bounds fails" (expect-failure
	(vector-ref #(a b c d e f) 7)))

(define-test "vector-ref with non-integer fails" (expect-failure
	(vector-ref #(a b c d e f) "4")))

(define-test "vector-set!" (expect #(1 1 2 1 1)
	; Need to make a new vector because vector literals are immutable
	(define test-vector (make-vector 5 1))
	(vector-set! test-vector 2 2)
	test-vector))

(define-test "vector-set! on vector literal fails" (expect-failure
	; Need to make a new vector because vector literals are immutable
	(vector-set! #(1 2 3 4 5) 2 2)))

(define-test "(vector-append) with no arguments" (expect #()
	(vector-append)))

(define-test "(vector-append) with single argument" (expect #(a b c)
	(vector-append #(a b c))))

(define-test "(vector-append) three vectors" (expect #(1 2 3 4 5 6)
	(vector-append #(1 2) #(3 4) #(5 6))))

(define-test "(vector-append) three empty vectors" (expect #()
	(vector-append #() #() #())))

(define-test "(vector-append) with non-vector fails" (expect-failure
	(vector-append '(1 2) '(3 4))))

(define-test "(vector->list)" (expect-success
  (assert-equal '() (vector->list #()))
  (assert-equal '(dah dah didah) (vector->list #(dah dah didah)))
  (assert-equal '(dah didah) (vector->list '#(dah dah didah) 1))
  (assert-equal '(dah) (vector->list '#(dah dah didah) 1 2))
  (assert-equal '() (vector->list '#(dah dah didah) 0 0))
  (assert-equal '() (vector->list '#(dah dah didah) 3 3))
  (assert-equal '(#(a b) #(c d) #(e f)) (vector->list #(#(a b) #(c d) #(e f))))))

(define-test "(vector->list) with backwards slice fails" (expect-failure
  (vector->list '#(dah dah didah) 2 1)))

(define-test "(vector->list) past end of vector fails" (expect-failure
  (vector->list '#(dah dah didah) 0 4)))

(define-test "(vector->list) with negative start index fails" (expect-failure
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

(define-test "(vector-copy) with backwards slice fails" (expect-failure
  (vector-copy '#(dah dah didah) 2 1)))

(define-test "(vector-copy) past end of vector fails" (expect-failure
  (vector-copy '#(dah dah didah) 0 4)))

(define-test "(vector-copy) with negative start index fails" (expect-failure
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

(define-test "(vector-fill!) on vector literal fails" (expect-failure
  (vector-fill! #(dah dah didah) #t)))

(define-test "(vector-fill!) with backwards slice fails" (expect-failure
  (vector-fill! (vector 'dah 'dah 'didah) #t 2 1)))

(define-test "(vector-fill!) past end of vector fails" (expect-failure
  (vector-fill! (vector 'dah 'dah 'didah) #t 0 4)))

(define-test "(vector-fill!) with negative start index fails" (expect-failure
  (vector-fill! (vector 'dah 'dah 'didah) #t -1)))
