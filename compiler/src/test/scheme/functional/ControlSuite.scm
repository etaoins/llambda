(define-test "(call/cc) escape procedure is a procedure" (expect #t
	(call/cc procedure?)))

(define-test "(call/cc) with non-procedure fails at compile time" (expect-compile-failure
	(call/cc 4)))

(define-test "(call/cc) with procedure of incorrect arity fails at compile time" (expect-compile-failure
	(call/cc cons)))

(define-test "captured (call/cc) escape procedure is a procedure" (expect #t
   (define captured-proc #f)
   (call/cc (lambda (escape-proc)
      (set! captured-proc escape-proc)))
   (procedure? captured-proc)))

(define-test "trivial (call/cc) invoking escape procedure" (expect 5
	(call/cc (lambda (return)
		(return 5)))))

(define-test "(call/cc) invoking escape procedure with multiple values" (expect 21
  (call-with-values
    (lambda ()
      (call/cc (lambda (return)
        (return * 7 3))))
    (lambda (proc . args)
      (apply proc args)))))

(define-test "trivial (call/cc) not invoking escape procedure" (expect 5
	(call/cc (lambda (return)
		5))))

(define-test "trivial (call/cc) only mutating escape procedure" (expect 5
	(call/cc (lambda (return)
    (set! return #f)
		5))))

(define-test "trivial (call/cc) mutating and invoking escape procedure" (expect -5
	(call/cc (lambda (return)
    (set! return -)
		(return 5)))))

(define-test "nested (call/cc) invoking both escape procedures" (expect 15
	(call/cc (lambda (outer-return)
		(outer-return (call/cc (lambda (inner-return)
			(inner-return 15)
		)))
	))))

(define-test "nested (call/cc) invoking only inner escape procedure" (expect -15
	(call/cc (lambda (outer-return)
		(- (call/cc (lambda (inner-return)
			(inner-return 15)
		)))
	))))

(define-test "nested (call/cc) invoking only outer escape procedure" (expect 15
	(call/cc (lambda (outer-return)
		(call/cc (lambda (inner-return)
			(outer-return 15)
		))
        'shouldntreach
	))))

(define-test "(call/cc) exiting from both branch sides" (expect one
	(call/cc (lambda (return)
		(if dynamic-true 
		  (return 'one)
		  (return 'two))
	))))

(define-test "(call/cc) exiting from one branch side" (expect one
	(call/cc (lambda (return)
		(if dynamic-true 
		  (return 'one)
		  #f)
	))))

(define-test "(define) doesn't accept multiple values" (expect-failure
  (define x (values 1 2 3))))

(define-test "(define) accepts single value produced with (values)" (expect test
  (define x (values 'test))
  x))

(define-test "(call-with-values) with single value" (expect -1
  (call-with-values * -)))

(define-test "(call-with-values) with wrong producer arity fails at compile time" (expect-compile-failure
  (call-with-values (lambda (extra-arg)) -)))

(define-test "(call-with-values) with multiple values" (expect 5
  (call-with-values (lambda () (values 4 5))
                    (lambda (a b) b))))

(define-test "(call-with-values) with mismatched arity fails" (expect-failure
  (call-with-values (lambda () (values 4 5))
                    (lambda (a b c) b))))

(define-test "(call-with-values) with wrong type fails" (expect-failure
  (import (llambda typed))
  (call-with-values (lambda () (values 4 5))
                    (lambda: ((a : <exact-integer>) (b : <flonum>)) b))))

(define-test "(call-with-values) with zero values returning multiple values" (expect (a b c)
  (call-with-values
    (lambda ()
      (call-with-values (lambda () (values))
                        (lambda () (values 'a 'b 'c))))
    (lambda values-list
      values-list))))

(define-test "multiple values returned from (if)" (expect (1 2 3 4)
  (define (return-multiple)
    (if dynamic-true
      (values 1 2 3 4)
      (values 4 5 6 7)))

  (call-with-values return-multiple
                    (lambda values-list values-list))))

(define-test "captured continuation called multiple times" (expect (0 1 2 3 4 5 6 7 8 9 10)
  (define result-list '())
  (define captured-cont #!unit)

  (define callcc-result
    (call/cc 
      (lambda (cont)
        (set! captured-cont cont)
        (cont 0))))

  ; Append the result on to our result list so we can check it later
  (set! result-list (append result-list (list callcc-result)))

  ; Keep calling until the value is 10
  (if (< callcc-result 10)
    (captured-cont (+ callcc-result 1)))

  result-list))
