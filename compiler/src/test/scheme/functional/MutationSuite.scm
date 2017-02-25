(define-test "trivial mutation" (expect 2
	(define x 1)
	(set! x 2)
	x))

(define-test "mutation across branches" (expect 3
	(define x 1)
	(if (dynamic-false)
	  (set! x 2)
	  (set! x 3))
	x))

(define-test "mutating unused top-level binding" (expect #!unit
	(define x 1)
	(set! x 2)))
