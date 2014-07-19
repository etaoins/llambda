(define-test "#t and #t are eqv" (expect #t
	(eqv? #t #t)))

(define-test "#f and #f are eqv" (expect #t
	(eqv? #f #f)))

(define-test "constant 'test and 'test are eqv" (expect #t
	(eqv? 'test 'test)))

(define-test "calculated 'test and 'test are eqv" (expect #t
	(eqv? (string->symbol "test") (string->symbol "test"))))

(define-test "constant 'test and calculated 'test are eqv" (expect #t
	(eqv? 'test (string->symbol "test"))))

(define-test "constant -163 and -163 are eqv" (expect #t
	(eqv? -163 -163)))

(define-test "calculated -163 and -163 are eqv" (expect #t
	(eqv? (- 163) (- 163))))

(define-test "constant 153.5 and 153.5 are eqv" (expect #t
	(eqv? 153.5 153.5)))

(define-test "calculatued 153.5 and 153.5 are eqv" (expect #t
	(eqv? (- -153.5) (- -153.5))))

(define-test "+inf.0 and +inf.0 are eqv" (expect #t
	(eqv? +inf.0 +inf.0)))

(define-test "#\a and #\a are eqv" (expect #t
	(eqv? #\a #\a)))

(define-test "'() and '() are eqv" (expect #t
	(eqv? '() '())))

(define-test "pairs in the same location are eqv" (expect #t
	(let ((var '(a b)))
		(eqv? var var))))

(define-test "vectors in the same location are eqv" (expect #t
	(let ((var #(1 2 3)))
		(eqv? var var))))

(define-test "records in the same location are eqv" (expect #t
	(define-record-type <unit> (unit) unit?)

	(let ((var (unit)))
		(eqv? var var))))

(define-test "procedures in the same location are eqv" (expect #t
	(let ((procecedure (lambda () 5)))
		(eqv? procecedure procecedure))))

(define-test "native functions are eqv" (expect #t
	(eqv? eqv? eqv?)))

(define-test "1 and #t are not eqv" (expect #f
	(eqv? 1 #t)))

(define-test "#t and #f are not eqv" (expect #f
	(eqv? #t #f)))

(define-test "'one and 'two are not eqv" (expect #f
	(eqv? 'one 'two)))

(define-test "-163 and -163.0 are not eqv" (expect #f
	(eqv? -163 -163.0)))

(define-test "-163 and 3435 are not eqv" (expect #f
	(eqv? -163 3435)))

(define-test "-163.5 and 3435.5 are not eqv" (expect #f
	(eqv? -163.5 3435.5)))

; This is undefined by R7RS
; However, if we do a pointer or intermediate value fast path comparison between NaN and itself we will return #t. If
; we want to be consistent we should return #t everywhere at all optimisation levels.
(define-test "literal +nan.0 and +nan.0 are eqv" (expect #t
  (eqv? +nan.0 +nan.0)))

(define-test "calculated +nan.0 and +nan.0 are eqv" (expect #t
  (eqv? (/ 0. 0.) (/ 0. 0.))))

(define-test "#\a and #\b are not eqv" (expect #f
	(eqv? #\a #\b)))

(define-test "'() and '(1 2 3) are not eqv" (expect #f
	(eqv? '() '(1 2 3))))

; With immutable pairs there's no distinction between constant and constructed lists
(cond-expand ((not immutable-pairs)
  (define-test "'two constructed lists are not eqv" (expect #f
    (eqv? (list 1 2 3) (list 1 2 3))))))

(define-test "'two constructed vectors are not eqv" (expect #f
	(eqv? (vector 1 2 3) (vector 1 2 3))))

(define-test "'two different procedures are not eqv" (expect #f
	; If these returned different values it would be legal to merge them
	(define (procecedure1) 1)
	(define (procecedure2) 2)
	(eqv? procecedure1 procecedure2)))

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

(define-test "constant lists are equal" (expect #t
	(equal? '(a) '(a))))

(define-test "constructed lists with equal content are equal" (expect #t
	(equal? (list 1 2 3) (list 1 2 3))))

(define-test "constructed lists withi different content are inequal" (expect #f
	(equal? (list 1 2 3) (list 1 2 5))))

(define-test "constant nested lists are equal" (expect #t
	(equal? '(a (b) c) '(a (b) c))))

(define-test "constant strings are equal" (expect #t
	(equal? "abc" "abc")))

(define-test "constant exact integers are equal" (expect #t
	(equal? 2 2)))

(define-test "constructed vectors with equal content are equal" (expect #t
	(equal? (make-vector 5 'a) (make-vector 5 'a))))

(define-test "constructed vectors with differing lengths are inequal" (expect #f
	(equal? (make-vector 5 'a) (make-vector 6 'a))))

(define-test "constructed vectors with differing content are inequal" (expect #f
	(equal? (make-vector 5 'a) (make-vector 5 'b))))

(define-test "constructed bytevectors with equal content are equal" (expect #t
	(equal? (make-bytevector 5 200) (make-bytevector 5 200))))

(define-test "constructed bytevectors with differing lengths are inequal" (expect #f
	(equal? (make-bytevector 5 200) (make-bytevector 6 200))))

(define-test "constructed bytevectors with differing content are inequal" (expect #f
	(equal? (make-bytevector 5 100) (make-bytevector 5 200))))
