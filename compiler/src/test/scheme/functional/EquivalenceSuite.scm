(define-test "boolean (eqv?)" (expect-success
	(assert-true (eqv? #t #t))
	(assert-true (eqv? #f #f))
	(assert-false (eqv? #f #t))))

(define-test "symbol (eqv?)" (expect-success
	(assert-true (eqv? 'test 'test))
	(assert-true (eqv? (string->symbol "test") (string->symbol "test")))
	(assert-true (eqv? 'test (string->symbol "test")))
	(assert-true (eqv?  (typed-dynamic 'test <symbol>) (typed-dynamic 'test <symbol>)))
	(assert-false (eqv? 'one 'two))
	(assert-false (eqv?  (typed-dynamic 'one <symbol>) (typed-dynamic 'two <symbol>)))))

(define-test "symbol union (eqv?)" (expect-success
  (import (llambda typed))

  ; These symbols are all of inline length
  (define-type <inline-symbol-union> (U 'one 'two 'three 'four 'five))

  (define inline-three (typed-dynamic 'three <inline-symbol-union>))
  (define inline-four  (typed-dynamic 'four  <inline-symbol-union>))
  (define inline-five  (typed-dynamic 'five  <inline-symbol-union>))

  ; Three can be distinguished purely by length
  (assert-true  (eqv? inline-three 'three))
  (assert-false (eqv? inline-three 'four))
  (assert-false (eqv? inline-three 'five))

  ; Four and five need to be distinguished with a byte lookup
  (assert-false (eqv? inline-four 'three))
  (assert-true  (eqv? inline-four 'four))
  (assert-false (eqv? inline-four 'five))

  (assert-false (eqv? inline-five 'three))
  (assert-false (eqv? inline-five 'four))
  (assert-true  (eqv? inline-five 'five))

  ; These symbols are all of the same length
  (define-type <same-length-symbol-union> (U 'value1 'value2 'value3))

  (define same-value1 (typed-dynamic 'value1 <same-length-symbol-union>))

  ; Three can be distinguished purely by length
  (assert-true  (eqv? same-value1 'value1))
  (assert-false (eqv? same-value1 'value2))
  (assert-false (eqv? same-value1 'value3))

  ; These symbols are all of heap length
  (define-type <heap-symbol-union> (U 'test-long-heap-symbol-one
                                      'test-long-heap-symbol-two
                                      'test-long-heap-symbol-three
                                      'test-long-heap-symbol-four
                                      'test-long-heap-symbol-five))

  (define heap-three (typed-dynamic 'test-long-heap-symbol-three <heap-symbol-union>))
  (define heap-four  (typed-dynamic 'test-long-heap-symbol-four  <heap-symbol-union>))
  (define heap-five  (typed-dynamic 'test-long-heap-symbol-five  <heap-symbol-union>))

  ; Three can be distinguished purely by length
  (assert-true  (eqv? heap-three 'test-long-heap-symbol-three))
  (assert-false (eqv? heap-three 'test-long-heap-symbol-four))
  (assert-false (eqv? heap-three 'test-long-heap-symbol-five))

  ; Four and five need to be distinguished with a byte lookup
  (assert-false (eqv? heap-four 'test-long-heap-symbol-three))
  (assert-true  (eqv? heap-four 'test-long-heap-symbol-four))
  (assert-false (eqv? heap-four 'test-long-heap-symbol-five))

  (assert-false (eqv? heap-five 'test-long-heap-symbol-three))
  (assert-false (eqv? heap-five 'test-long-heap-symbol-four))
  (assert-true  (eqv? heap-five 'test-long-heap-symbol-five))))

(define-test "numeric (eqv?)" (expect-success
  (import (llambda typed))

	(assert-true (eqv? -163 -163))
	(assert-true (eqv? (- 163) (- 163)))
	(assert-true (eqv? 153.5 153.5))
	(assert-true (eqv? (- -153.5) (- -153.5)))
	(assert-true (eqv? +inf.0 +inf.0))
	(assert-false (eqv? -163 -163.0))
	(assert-false (eqv? -163 3435))
	(assert-false (eqv? -163.5 3435.5))
  (assert-false (eqv? +nan.0 (typed-dynamic 0.0 <flonum>)))

  (assert-false (eqv? 0.0 -0.0))
  (assert-false (eqv? 0.0 (typed-dynamic -0.0 <flonum>)))
  (assert-false (eqv? -0.0 (typed-dynamic 0.0 <flonum>)))
  (assert-false (eqv? (typed-dynamic 0.0 <flonum>) (typed-dynamic -0.0 <flonum>)))

  ; This is undefined by R7RS
  ; However, if we do a pointer or intermediate value fast path comparison between NaN and itself we will return #t. If
  ; we want to be consistent we should return #t everywhere at all optimisation levels.
  (assert-true (eqv? +nan.0 +nan.0))
  (assert-true (eqv? (/ 0. 0.) (/ 0. 0.)))
  (assert-true (eqv? +nan.0 (typed-dynamic +nan.0 <flonum>)))))

(define-test "char (eqv?)" (expect-success
	(assert-true (eqv? #\a #\a))
	(assert-true (eqv? #\a (integer->char (char->integer #\a))))
	(assert-false (eqv? #\a #\b))))

(define-test "null (eqv?)" (expect-success
	(assert-true (eqv? '() '()))))

(define-test "pair (eqv?)" (expect-success
	(let ((var '(a b)))
		(assert-true (eqv? var var)))

  (assert-false (eqv? '() '(1 2 3)))

  ; With immutable pairs there's no distinction between constant and constructed lists
  (cond-expand ((not immutable-pairs)
    (assert-false (eqv? (list 1 2 3) (list 1 2 3)))))))

(define-test "vector (eqv?)" (expect-success
	(let ((var #(1 2 3)))
		(assert-true (eqv? var var)))

	(assert-false (eqv? (vector 1 2 3) (vector 1 2 3)))))

(define-test "record (eqv?)" (expect-success
	(define-record-type <unit> (unit) unit?)

	(let ((var (unit)))
		(assert-true (eqv? var var)))

  (assert-false (eqv? (unit) (unit)))))

(define-test "procedure (eqv?)" (expect-success
	(let ((procecedure (lambda () 5)))
		(assert-true (eqv? procecedure procecedure)))

  (assert-true (eqv? eqv? eqv?))

  ; If these returned different values it would be legal to merge them
  (define (procecedure1) 1)
  (define (procecedure2) 2)
  (assert-false (eqv? procecedure1 procecedure2))))

(define-test "1 and #t are not eqv" (expect #f
	(eqv? 1 #t)))

(define-test "constant 'a and 'a are eq" (expect #t
	(eq? 'a 'a)))

(cond-expand ((not immutable-pairs)
  (define-test "constructed lists are not eq" (expect #f
    (eq? (list 'a) (list 'a))))))

(define-test "'() and '() are eq" (expect #t
	(eq? '() '())))

(define-test "native functions are eq" (expect #t
	(eq? car car)))

(define-test "values in the same argument are eq" (expect-success
  (define (arg-is-self-eq? x) (eq? x x))

  (assert-true (arg-is-self-eq? '(a)))
  (assert-true (arg-is-self-eq? #()))
  (assert-true (arg-is-self-eq? #u8(1 2 3 4)))
  (assert-true (arg-is-self-eq? (lambda (x) x)))))

(define-test "values in the same variable are eq" (expect-success
  (define test-list '(a))
  (assert-true (eq? test-list test-list))

  (define test-vec #())
  (assert-true (eq? test-vec test-vec))

  (define test-bytevec #u8(1 2 3 4))
  (assert-true (eq? test-bytevec test-bytevec))

  (define test-proc (lambda (x) x))
  (assert-true (eq? test-proc test-proc))))

(define-test "calculated 'test and 'test are eq" (expect #t
	(eq? (string->symbol "test") (string->symbol "test"))))

(define-test "constant 'a and 'a are equal" (expect #t
	(equal? 'a 'a)))

(define-test "list (equal?)" (expect-success
	(assert-true (equal? '(a) '(a)))
	(assert-true (equal? (list 1 2 3) (list 1 2 3)))
	(assert-false (equal? (list 1 2 3) (list 1 2 5)))
	(assert-true (equal? '(a (b) c) '(a (b) c)))))

(define-test "constant strings are equal" (expect #t
	(equal? "abc" "abc")))

(define-test "constant exact integers are equal" (expect #t
	(equal? 2 2)))

(define-test "vector (equal?)" (expect-success
	(assert-true (equal? (make-vector 5 'a) (make-vector 5 'a)))
	(assert-false (equal? (make-vector 5 'a) (make-vector 6 'a)))
	(assert-false (equal? (make-vector 5 'a) (make-vector 5 'b)))))

(define-test "bytevector (equal?)" (expect-success
	(assert-true (equal? (make-bytevector 5 200) (make-bytevector 5 200)))
	(assert-false (equal? (make-bytevector 5 200) (make-bytevector 6 200)))
	(assert-false (equal? (make-bytevector 5 100) (make-bytevector 5 200)))))
