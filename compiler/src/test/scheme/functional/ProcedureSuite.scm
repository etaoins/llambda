(define-test "+ is a procedure" (expect #t
	(procedure? +)))

(define-test "#f is not a procedure" (expect #f
	(procedure? #f)))

(define-test "non-constant procedure values can be applied" (expect 10
	(import (llambda test-util))

	(define math-op
		; This is + but our optimizer won't know that
		; That means this has to be converted to a function value at runtime

		; Also note that + and / have different signatures which will have to be
		; normalized when they're boxed. This doesn't matter for our current
		; implementation but it may matter if we try to be more tricky
		(if undecided-true + /))

	(math-op 2 3 5)))

(define-test "datum cells can be applied" (expect 10
	(import (llambda test-util))

	((typeless-cell -) 80 50 20)))

(define-test "procedure returning nothing" (expect #!unspecific
	(define (return-nothing))
	(return-nothing)))

(define-test "procedure returning single value" (expect 7
	(define (return-7) 7)
	(return-7)))

(define-test "procedure returning its only argument" (expect 7
	(define (return-value value) value)
	(return-value 7)))

(define-test "procedure adding its arguments" (expect 7
	(define (add-two-values a b) (+ a b))
	(add-two-values 4 3)))
