(define-test "(call/cc) escape procedure is a procedure" (expect #t
	(call/cc procedure?)))

(define-test "captured (call/cc) escape procedure is a procedure" (expect #t
   (define captured-proc #f)
   (call/cc (lambda (escape-proc)
      (set! captured-proc escape-proc)))
   (procedure? captured-proc)))

(define-test "trivial (call/cc) invoking escape procedure" (expect 5
	(call/cc (lambda (return)
		(return 5)))))

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
	(import (llambda test-util))
	(call/cc (lambda (return)
		(if dynamic-true 
		  (return 'one)
		  (return 'two))
	))))

(define-test "(call/cc) exiting from one branch side" (expect one
	(import (llambda test-util))
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

(define-test "(call-with-values) with multiple values" (expect 5
  (call-with-values (lambda () (values 4 5))
                    (lambda (a b) b))))

(define-test "(call-with-values) with mismatched arity fails" (expect-failure
  (call-with-values (lambda () (values 4 5))
                    (lambda (a b c) b))))

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
