(define-test "no-op (cast)" (expect 10
	(import (llambda typed))
	(cast 10 <exact-integer>)))

(define-test "no-op (ann)" (expect 10
	(import (llambda typed))
	(ann 10 <exact-integer>)))

(define-test "(cast) cannot convert int to flonum" (expect-compile-failure
	(import (llambda typed))
	(cast 50 <flonum>)))

(define-test "(ann) cannot convert int to flonum" (expect-compile-failure
	(import (llambda typed))
	(ann 50 <flonum>)))

(define-test "statically impossible (cast) fails at compile time" (expect-compile-failure
	(import (llambda typed))
	(cast #t <exact-integer>)))

(define-test "statically impossible (ann) fails at compile time" (expect-compile-failure
	(import (llambda typed))
	(ann #t <exact-integer>)))

(define-test "dynamically impossible (cast) fails at runtime" (expect-runtime-failure
	(import (llambda typed))
	(cast (typeless-cell #t) <exact-integer>)))

(define-test "dynamically impossible (ann) fails at compile time" (expect-compile-failure
	(import (llambda typed))
	(ann (typeless-cell #t) <exact-integer>)))

(define-test "dynamically possible (cast) succeeds" (expect #t
	(import (llambda typed))
	(cast (typeless-cell #t) <boolean>)))

(define-test "dynamically possible (ann) fails at compile time" (expect-compile-failure
	(import (llambda typed))
	(ann (typeless-cell #t) <boolean>)))

(define-test "expressions can be cast as record types" (expect #t
	(import (llambda typed))
	(import (llambda test-util))

	(define-record-type <type1> (type1) type1?)

	(type1? (cast (typeless-cell (type1)) <type1>))))
