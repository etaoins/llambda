(define-test "no-op cast" (expect 10
	(import (llambda typed))
	(cast 10 <integer>)))

; Do we want this?
(define-test "cast cannot convert int to flonum" (expect-failure
	(import (llambda typed))
	(cast 50 <flonum>)))

(define-test "impossible cast fails" (expect-failure
	(import (llambda typed))
	(cast #t <integer>)))

(define-test "cast can convert typeless datums" (expect #t
	(import (llambda typed))
	(import (llambda test-util))
	(cast (typeless-cell #t) <boolean>)))

(define-test "expressions can be annotated as record types" (expect #t
	(import (llambda typed))
	(import (llambda test-util))

	(define-record-type <type1> (type1) type1?)

	(type1? (cast (typeless-cell (type1)) <type1>))))
