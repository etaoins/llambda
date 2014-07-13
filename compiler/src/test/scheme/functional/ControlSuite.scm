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
		(if undecided-true 
		  (return 'one)
		  (return 'two))
	))))

(define-test "(call/cc) exiting from one branch side" (expect one
	(import (llambda test-util))
	(call/cc (lambda (return)
		(if undecided-true 
		  (return 'one)
		  #f)
	))))
