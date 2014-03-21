(define-test "trivial (call/cc)" (expect 5
	(call/cc (lambda (return)
		(return 5)))))

(define-test "nested (call/cc)" (expect 15
	(call/cc (lambda (outer-return)
		(outer-return (call/cc (lambda (inner-return)
			(inner-return 15)
		)))
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
