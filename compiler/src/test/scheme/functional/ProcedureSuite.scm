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

(define-test "boxed datums can be applied" (expect 10
	(import (llambda test-util))

	((typeless-boxed -) 80 50 20)))
