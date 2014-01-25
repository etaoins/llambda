(define-test "parameters are procedures" (expect #t
	(define param (make-parameter 18))
	(procedure? param)))

(define-test "parameters return initial value if not parameterized" (expect 18
	(define param (make-parameter 18))
	(param)))

(define-test "trivial parameterize" (expect 50
	(define param (make-parameter 0))

	(parameterize ((param 50))
		(param))))

(define-test "nested parameterize" (expect hello
	(define param (make-parameter 0))

	(parameterize ((param 50))
		(parameterize ((param 'hello))
			(param)))))

(define-test "multiple parameter parameterize" (expect (newOne two newThree) 
	(define param1 (make-parameter 'one))
	(define param2 (make-parameter 'two))
	(define param3 (make-parameter 'three))

	(parameterize ((param1 'newOne) (param3 'newThree))
		(list (param1) (param2) (param3)))))

(define-test "parameterize restores dynamic environment afterwards" (expect 0
	(define param (make-parameter 0))

	(parameterize ((param 50))
		(param))

	(param)))

(define-test "trivial dynamic-wind" (expect (56.1 . 60)
	(define testValue 0)

	(define returnValue
	  (dynamic-wind
		; Add 10 before the thunk - testValue will then be 10
		(lambda () (set! testValue (+ testValue 10)))
		; Multiply by 4 in the thunk and return 56.1 - testValue will then be 40
		(lambda () (set! testValue (* testValue 4)) 56.1)
		; Add 20 - testValue will then be 60
		(lambda () (set! testValue (+ testValue 20)))))

	(cons returnValue testValue)))

