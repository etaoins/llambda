(define-test "no-op (cast)" (expect 10
  (import (llambda typed))
  (cast 10 <integer>)))

(define-test "no-op (ann)" (expect 10
  (import (llambda typed))
  (ann 10 <integer>)))

(define-test "(cast) cannot convert int to flonum" (expect-compile-error type-error?
  (import (llambda typed))
  (cast 50 <flonum>)))

(define-test "(ann) cannot convert int to flonum" (expect-compile-error type-error?
  (import (llambda typed))
  (ann 50 <flonum>)))

(define-test "(cast) can convert int to unit" (expect-static-success
  (import (llambda typed))
  (cast 50 <unit>)))

(define-test "(ann) can convert int to unit" (expect-static-success
  (import (llambda typed))
  (ann 50 <unit>)))

(define-test "statically impossible (cast) fails at compile time" (expect-compile-error type-error?
  (import (llambda typed))
  (cast #t <integer>)))

(define-test "statically impossible (ann) fails at compile time" (expect-compile-error type-error?
  (import (llambda typed))
  (ann #t <integer>)))

(define-test "(ann) on a procedure" (expect-static-success
  (import (llambda typed))
  (ann + (-> <number> <number> <number>))))

(define-test "dynamically impossible (cast) fails at runtime" (expect-success
  (import (llambda typed))
  (import (llambda error))

  (assert-raises type-error?
    (cast (typeless-cell #t) <integer>))))

(define-test "dynamically impossible (ann) fails at compile time" (expect-compile-error type-error?
  (import (llambda typed))
  (ann (typeless-cell #t) <integer>)))

(define-test "dynamically possible (cast) succeeds" (expect #t
  (import (llambda typed))
  (cast (typeless-cell #t) <boolean>)))

(define-test "dynamically possible (ann) fails at compile time" (expect-compile-error type-error?
  (import (llambda typed))
  (ann (typeless-cell #t) <boolean>)))

(define-test "dynamically possible procedure (ann) fails at compile time" (expect-compile-error type-error?
  (import (llambda typed))

  (: test-proc (-> <integer> <number>))
  (define (test-proc x) x)

  (ann test-proc (-> <number> <number>))))

(define-test "expressions can be cast as record types" (expect #t
  (import (llambda typed))

  (define-record-type <type1> (type1) type1?)

  (type1? (cast (typeless-cell (type1)) <type1>))))
