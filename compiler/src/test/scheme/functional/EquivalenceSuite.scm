(define-test "boolean (eqv?)" (expect-static-success
  (assert-true (eqv? #t #t))
  (assert-true (eqv? #f #f))
  (assert-false (eqv? #f #t))))

(define-test "dynamic boolean (eqv?)" (expect-success
  ; This is a native predicate value
  (define native-false (input-port? #f))

  (assert-false (eqv? #t native-false))
  (assert-false (eqv? native-false #t))
  (assert-true  (eqv? #f native-false))
  (assert-true  (eqv? native-false #f))
  (assert-true  (eqv? native-false native-false))))

(define-test "static symbol (eqv?)" (expect-static-success
  (assert-true (eqv? 'test 'test))
  (assert-false (eqv? 'one 'two))
  (assert-false (eqv? 'one '()))))

(define-test "dynamic symbol (eqv?)" (expect-success
  (assert-true (eqv? (string->symbol "test") (string->symbol "test")))
  (assert-true (eqv? 'test (string->symbol "test")))
  (assert-true (eqv?  (typed-dynamic 'test <symbol>) (typed-dynamic 'test <symbol>)))
  (assert-false (eqv?  (typed-dynamic 'one <symbol>) (typed-dynamic 'two <symbol>)))))

(define-test "static string (eqv?)" (expect-static-success
  (assert-true  (eqv? "one" "one"))
  (assert-false (eqv? "one" "two"))
  (assert-false (eqv? "one" '()))))

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
  (define-type <heap-symbol-union> (U 'test-extra-long-heap-symbol-one
                                      'test-extra-long-heap-symbol-two
                                      'test-extra-long-heap-symbol-three
                                      'test-extra-long-heap-symbol-four
                                      'test-extra-long-heap-symbol-five))

  (define heap-three (typed-dynamic 'test-extra-long-heap-symbol-three <heap-symbol-union>))
  (define heap-four  (typed-dynamic 'test-extra-long-heap-symbol-four  <heap-symbol-union>))
  (define heap-five  (typed-dynamic 'test-extra-long-heap-symbol-five  <heap-symbol-union>))

  ; Three can be distinguished purely by length
  (assert-true  (eqv? heap-three 'test-extra-long-heap-symbol-three))
  (assert-false (eqv? heap-three 'test-extra-long-heap-symbol-four))
  (assert-false (eqv? heap-three 'test-extra-long-heap-symbol-five))

  ; Four and five need to be distinguished with a byte lookup
  (assert-false (eqv? heap-four 'test-extra-long-heap-symbol-three))
  (assert-true  (eqv? heap-four 'test-extra-long-heap-symbol-four))
  (assert-false (eqv? heap-four 'test-extra-long-heap-symbol-five))

  (assert-false (eqv? heap-five 'test-extra-long-heap-symbol-three))
  (assert-false (eqv? heap-five 'test-extra-long-heap-symbol-four))
  (assert-true  (eqv? heap-five 'test-extra-long-heap-symbol-five))

  ; This contains both heap and inline symbols
  (define-type <mixed-symbol-union> (U 'six
                                       'test-extra-long-heap-symbol-six
                                       'seven
                                       'test-extra-long-heap-symbol-seven))

  (define inline-six (typed-dynamic 'six <mixed-symbol-union>))
  (define heap-six (typed-dynamic 'test-extra-long-heap-symbol-six <mixed-symbol-union>))
  (define inline-seven (typed-dynamic 'seven <mixed-symbol-union>))
  (define heap-seven (typed-dynamic 'test-extra-long-heap-symbol-seven <mixed-symbol-union>))

  (assert-true  (eqv? inline-six 'six))
  (assert-false (eqv? inline-six 'test-extra-long-heap-symbol-six))
  (assert-false (eqv? inline-six 'seven))
  (assert-false (eqv? inline-six 'test-extra-long-heap-symbol-seven))

  (assert-false (eqv? heap-six 'six))
  (assert-true  (eqv? heap-six 'test-extra-long-heap-symbol-six))
  (assert-false (eqv? heap-six 'seven))
  (assert-false (eqv? heap-six 'test-extra-long-heap-symbol-seven))

  (assert-false (eqv? inline-seven 'six))
  (assert-false (eqv? inline-seven 'test-extra-long-heap-symbol-six))
  (assert-true  (eqv? inline-seven 'seven))
  (assert-false (eqv? inline-seven 'test-extra-long-heap-symbol-seven))

  (assert-false (eqv? heap-seven 'six))
  (assert-false (eqv? heap-seven 'test-extra-long-heap-symbol-six))
  (assert-false (eqv? heap-seven 'seven))
  (assert-true  (eqv? heap-seven 'test-extra-long-heap-symbol-seven))))

(define-test "static numeric (eqv?)" (expect-static-success
  (assert-true (eqv? -163 -163))
  (assert-true (eqv? (- 163) (- 163)))
  (assert-true (eqv? 153.5 153.5))
  (assert-true (eqv? (- -153.5) (- -153.5)))
  (assert-true (eqv? +inf.0 +inf.0))
  (assert-false (eqv? -163 -163.0))
  (assert-false (eqv? -163 3435))
  (assert-false (eqv? -163.5 3435.5))
  (assert-false (eqv? 0.0 -0.0))))

(define-test "dynamic numeric (eqv?)" (expect-success
  (import (llambda typed))

  (assert-false (eqv? +nan.0 (typed-dynamic 0.0 <flonum>)))

  (assert-false (eqv? 0.0 (typed-dynamic -0.0 <flonum>)))
  (assert-false (eqv? (typed-dynamic 0.0 <flonum>) -0.0))
  (assert-false (eqv? (typed-dynamic 0.0 <flonum>) (typed-dynamic -0.0 <flonum>)))

  ; This is undefined by R7RS
  ; However, if we do a pointer or intermediate value fast path comparison between NaN and itself we will return #t. If
  ; we want to be consistent we should return #t everywhere at all optimisation levels.
  (assert-true (eqv? +nan.0 +nan.0))
  (assert-true (eqv? (/ 0.0 0.0) (/ 0.0 0.0)))
  (assert-true (eqv? +nan.0 (typed-dynamic +nan.0 <flonum>)))))

(define-test "char (eqv?)" (expect-static-success
  (assert-true (eqv? #\a #\a))
  (assert-true (eqv? #\a (integer->char (char->integer #\a))))
  (assert-false (eqv? #\a #\b))))

(define-test "null (eqv?)" (expect-static-success
  (assert-true (eqv? '() '()))))

(define-test "pair (eqv?)" (expect-static-success
  (let ((var '(a b)))
    (assert-true (eqv? var var)))

  (assert-false (eqv? '() '(1 2 3)))))

(define-test "vector (eqv?)" (expect-success
  (let ((var #(1 2 3)))
    (assert-true (eqv? var var)))

  (assert-false (eqv? (vector 1 2 3) (vector 1 2 3)))))

(define-test "record (eqv?)" (expect-success
  (define-record-type <immutable> (immutable) immutable?)
  (define-record-type <mutable> (mutable) mutable?
                      (field mutable-field set-mutable-field!))

  (let ((var (immutable)))
    (assert-true (eqv? var var)))

  ; This is an immutable record - we should share these instances
  (assert-true (eqv? (immutable) (immutable)))

  (let ((var (mutable)))
    (assert-true (eqv? var var)))

  ; This is a mutable record - we shouldn't share instances
  (assert-false (eqv? (mutable) (mutable)))))

(define-test "procedure (eqv?)" (expect-success
  (let ((procecedure (lambda () 5)))
    (assert-true (eqv? procecedure procecedure)))

  (assert-true (eqv? eqv? eqv?))

  ; If these returned the same value it would be legal to merge them
  (define (procecedure1) 1)
  (define (procecedure2) 2)
  (assert-false (eqv? procecedure1 procecedure2))))

(define-test "1 and #t are not eqv" (expect #f
  (eqv? 1 #t)))

(define-test "constant 'a and 'a are eqv" (expect #t
  (eqv? 'a 'a)))

(define-test "'() and '() are eqv" (expect #t
  (eqv? '() '())))

(define-test "native functions are eqv" (expect #t
  (eqv? car car)))

(define-test "values in the same argument are eqv" (expect-static-success
  (define (arg-is-self-eqv? x) (eqv? x x))

  (assert-true (arg-is-self-eqv? '(a)))
  (assert-true (arg-is-self-eqv? #()))
  (assert-true (arg-is-self-eqv? #u8(1 2 3 4)))
  (assert-true (arg-is-self-eqv? (lambda (x) x)))))

(define-test "values in the same variable are eqv" (expect-static-success
  (define test-list '(a))
  (assert-true (eqv? test-list test-list))

  (define test-vec #())
  (assert-true (eqv? test-vec test-vec))

  (define test-bytevec #u8(1 2 3 4))
  (assert-true (eqv? test-bytevec test-bytevec))

  (define test-proc (lambda (x) x))
  (assert-true (eqv? test-proc test-proc))))

(define-test "calculated 'test and 'test are eqv" (expect #t
  (eqv? (string->symbol "test") (string->symbol "test"))))

(define-test "constant 'a and 'a are equal" (expect #t
  (equal? 'a 'a)))

(define-test "list (equal?)" (expect-static-success
  (assert-true (equal? '(a) '(a)))
  (assert-true (equal? (list 1 2 3) (list 1 2 3)))
  (assert-false (equal? (list 1 2 3) (list 1 2 5)))
  (assert-false (equal? '(1 2 3 . 4) '(1 2 3 . 5)))
  (assert-true (equal? '(a (b) c) '(a (b) c)))))

(define-test "dynamic pair (equal?)" (expect-success
  (assert-true  (equal? '(1 . 2) (cons 1 (typed-dynamic 2 <integer>))))
  (assert-false (equal? '(1 . 2) (cons 1 (typed-dynamic 1 <integer>))))))

(define-test "constant strings are equal" (expect-static-success
  (assert-true (equal? "abc" "abc"))))

(define-test "constant integers are equal" (expect-static-success
  (assert-true (equal? 2 2))))

(define-test "static vector (equal?)" (expect-static-success
  (assert-true  (equal? #(1 2 (3 4)) #(1 2 (3 4))))
  (assert-false (equal? #(1 2 (3 4)) #(1 2 (3 4 5))))
  (assert-true  (equal? #(1 2 3 4) #(1 2 3 4)))
  (assert-false (equal? #(1 2 3 4) #(1 2 3 4 5)))
  (assert-false (equal? #(1 2 3 4) #(1 2 3 5)))
  (assert-true  (equal? #(1 2 3 #(4 5)) #(1 2 3 #(4 5))))
  (assert-false (equal? #(1 2 3 #(4 5)) '(1 2 3 #(4 6))))
  (assert-false (equal? #(1 2 3 4) #f))))

(define-test "dynamic vector (equal?)" (expect-success
  (assert-true (equal? (make-vector 5 'a) (make-vector 5 'a)))
  (assert-false (equal? (make-vector 5 'a) (make-vector 6 'a)))
  (assert-false (equal? (make-vector 5 'a) (make-vector 5 'b)))))

(define-test "static bytevector (equal?)" (expect-static-success
  (assert-true (equal? #u8(1 2 3 4) #u8(1 2 3 4)))
  (assert-false (equal? #u8(1 2 3 4) #u8(1 2 3 4 5)))
  (assert-false (equal? #u8(1 2 3 4) #u8(1 2 3 5)))
  (assert-false (equal? #u8(1 2 3 4) #f))))

(define-test "dynamic bytevector (equal?)" (expect-success
  (assert-true (equal? (make-bytevector 5 200) (make-bytevector 5 200)))
  (assert-false (equal? (make-bytevector 5 200) (make-bytevector 6 200)))
  (assert-false (equal? (make-bytevector 5 100) (make-bytevector 5 200)))))

(define-test "dynamic error object (equal?)" (expect-success
  (import (llambda error))

  (define error-1 (guard (e (else e)) (raise-file-error "one")))
  (define error-2 (guard (e (else e)) (raise-file-error "two")))
  (define error-3 (guard (e (else e)) (raise-file-error "one" 1 2 3)))
  (define error-4 (guard (e (else e)) (raise-read-error "one")))
  (define error-5 (guard (e (else e)) (raise-file-error "one")))

  (assert-true  (equal? error-1 error-1))
  (assert-false (equal? error-1 error-2))
  (assert-false (equal? error-1 error-3))
  (assert-false (equal? error-1 error-3))
  (assert-false (equal? error-1 error-4))
  (assert-true  (equal? error-1 error-5))

  (assert-true  (equal? error-2 error-2))
  (assert-false (equal? error-2 error-3))
  (assert-false (equal? error-2 error-4))
  (assert-false (equal? error-2 error-5))

  (assert-true  (equal? error-3 error-3))
  (assert-false (equal? error-3 error-4))
  (assert-false (equal? error-3 error-5))

  (assert-true  (equal? error-4 error-4))
  (assert-false (equal? error-4 error-5))

  (assert-true  (equal? error-5 error-5))))

(define-test "static record (equal?)" (expect-static-success
  (import (llambda typed))

  (define-record-type <constant-empty> (constant-empty) constant-empty?)
  (define-record-type <constant-child> <constant-empty> (constant-child) constant-child?)

  (define-record-type <constant-inline> (constant-inline field1) constant-inline?
                      ([field1 : <flonum>] constant-inline-field1))

  (define-record-type <constant-ool> (constant-ool field1 field2 field3 field4) constant-ool?
                      ([field1 : <flonum>] constant-ool-field1)
                      ([field2 : <integer>] constant-ool-field2)
                      ([field3 : <flonum>] constant-ool-field3)
                      ([field4 : <vector>] constant-ool-field4))

  (assert-true  (equal? (constant-empty) (constant-empty)))

  (assert-true  (equal? (constant-inline 1.0) (constant-inline 1.0)))
  (assert-true  (equal? (constant-inline +nan.0) (constant-inline +nan.0)))
  (assert-false (equal? (constant-inline 1.0) (constant-inline -1.0)))
  (assert-false (equal? (constant-inline 1.0) (constant-inline +nan.0)))

  (assert-true  (equal? (constant-ool 1.0 2 3.0 #(4)) (constant-ool 1.0 2 3.0 #(4))))
  (assert-false (equal? (constant-ool 1.0 2 3.0 #(4)) (constant-ool 1.0 2 3.0 #(5))))
  (assert-false (equal? (constant-ool 1.0 2 +nan.0 #(4)) (constant-ool 1.0 2 3.0 #(4))))
  (assert-false (equal? (constant-ool 1.0 2 3.0 #(4)) (constant-ool 1.0 3 3.0 #(4))))

  (assert-false (equal? (constant-empty) (constant-child)))))

(define-test "dynamic record (equal?)" (expect-success
  (import (llambda typed))

  (define-record-type <unboxed-inline> (unboxed-inline field1) unboxed-inline?
                      ([field1 : <flonum>] unboxed-inline-field1))

  (define-record-type <unboxed-ool> (unboxed-ool field1 field2 field3 field4) unboxed-ool?
                      ([field1 : <flonum>] unboxed-ool-field1)
                      ([field2 : <integer>] unboxed-ool-field2)
                      ([field3 : <flonum>] unboxed-ool-field3)
                      ([field4 : <integer>] unboxed-ool-field4))

  (define-record-type <boxed-inline> (boxed-inline field1) boxed-inline?
                      ([field1 : <bytevector>] boxed-inline-field1))

  (define-record-type <boxed-ool> (boxed-ool field1 field2 field3 field4) boxed-ool?
                      ([field1 : <flonum>] boxed-ool-field1)
                      ([field2 : <integer>] boxed-ool-field2)
                      ([field3 : <flonum>] boxed-ool-field3)
                      ([field4 : <vector>] boxed-ool-field4))

  (define-record-type <mutable-inline> (mutable-inline field1) mutable-inline?
                      ([field1 : <integer>] mutable-inline-field1 set-mutable-inline-field1!))

  (assert-true  (equal? (unboxed-inline 1.0) (unboxed-inline (typeless-cell 1.0))))
  (assert-true  (equal? (unboxed-inline (typeless-cell +nan.0)) (unboxed-inline +nan.0)))
  (assert-false (equal? (unboxed-inline 1.0) (unboxed-inline (typeless-cell -1.0))))
  (assert-false (equal? (unboxed-inline (typeless-cell 1.0)) (unboxed-inline +nan.0)))

  (assert-true  (equal? (unboxed-ool (typeless-cell 1.0) 2 3.0 4) (unboxed-ool 1.0 2 3.0 4)))
  (assert-false (equal? (unboxed-ool 1.0 2 3.0 4) (unboxed-ool 1.0 (typeless-cell 2) 3.0 5)))
  (assert-false (equal? (unboxed-ool 1.0 2 (typeless-cell +nan.0) 4) (unboxed-ool 1.0 2 3.0 4)))
  (assert-false (equal? (unboxed-ool 1.0 2 3.0 4) (unboxed-ool 1.0 3 3.0 (typeless-cell 4))))

  (assert-true  (equal? (boxed-inline (typeless-cell #u8(1 2 3))) (boxed-inline #u8(1 2 3))))
  (assert-false (equal? (boxed-inline (typeless-cell #u8(1 2 3))) (boxed-inline #u8(3 2 1))))

  (assert-true  (equal? (boxed-ool (typeless-cell 1.0) 2 3.0 #(4)) (boxed-ool 1.0 2 3.0 #(4))))
  (assert-false (equal? (boxed-ool 1.0 2 3.0 #(4)) (boxed-ool 1.0 (typeless-cell 2) 3.0 #(5))))
  (assert-false (equal? (boxed-ool 1.0 2 (typeless-cell +nan.0) #(4)) (boxed-ool 1.0 2 3.0 #(4))))
  (assert-false (equal? (boxed-ool 1.0 2 3.0 #(4)) (boxed-ool 1.0 3 3.0 (typeless-cell #(4)))))

  (define mutable1 (mutable-inline 1))
  (define mutable2 (mutable-inline 2))

  (assert-false (equal? mutable1 mutable2))
  (set-mutable-inline-field1! mutable1 2)
  (assert-true (equal? mutable1 mutable2))))
