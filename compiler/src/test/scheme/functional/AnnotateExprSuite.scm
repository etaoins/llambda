(define-test "no-op annotation" (expect 10
	(import (llambda typed))
	(ann 10 <integer>)))

; Do we want this?
(define-test "annotation cannot convert int to flonum" (expect-failure
	(import (llambda typed))
	(ann 50 <flonum>)))

(define-test "impossible annotation fails" (expect-failure
	(import (llambda typed))
	(ann #t <integer>)))

(define-test "annotation can convert typeless datums" (expect #t
	(import (llambda typed))
	(import (llambda test-util))
	(ann (typeless-cell #t) <boolean>)))

(define-test "expressions can be annotated as record types" (expect #t
	(import (llambda typed))
	(import (llambda test-util))

	(define-record-type <type1> (type1) type1?)

	(type1? (ann (typeless-cell (type1)) <type1>))))
