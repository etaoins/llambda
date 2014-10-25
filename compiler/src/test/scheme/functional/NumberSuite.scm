(define-test "(number?)" (expect-success
  (assert-true  (number? 4))
  (assert-true  (number? (typeless-cell 4)))

  (assert-true  (number? -5.0))
  (assert-true  (number? (typeless-cell -5.0)))

  (assert-false (number? '()))
  (assert-false (number? (typeless-cell '())))))

(define-test "(real?)" (expect-success
  (assert-true  (real? 4))
  (assert-true  (real? -5.0))
  (assert-false (real? '()))))

(define-test "(rational?)" (expect-success
  (assert-true  (rational? 4))
  (assert-true  (rational? -5.0))
  (assert-true  (rational? 6/10))
  (assert-false (rational? +inf.0))
  (assert-false (rational? +nan.0))
  (assert-false (rational? '()))))

(define-test "(complex?)" (expect-success
  (assert-true  (complex? 4))
  (assert-true  (complex? -5.0))
  (assert-false (complex? '()))))

(define-test "(integer?)" (expect-success
  (assert-true  (integer? 4))
  (assert-true  (integer? -5.0))
  (assert-false (integer? -5.5))
  (assert-false (integer? +nan.0))
  (assert-false (integer? '()))))

(define-test "(exact?)" (expect-success
  (assert-false (exact? 3.0))
  (assert-true  (exact? 3.))))

(define-test "exact? fails with non-numbers" (expect-failure
  (exact? 'notanumber)))

(define-test "(inexact?)" (expect-success
  (assert-true  (inexact? 3.0))
  (assert-false (inexact? 3.))))

(define-test "(inexact?) fails with non-numbers" (expect-failure
  (inexact? 'notanumber)))

(define-test "(exact-integer?)" (expect-success
  (assert-true (exact-integer? 32))
  (assert-false (exact-integer? 32.0))))

(define-test "(exact)" (expect-success
  (assert-equal -32 (exact -32.0))
  (assert-equal 64 (exact 64))))

(define-test "(exact 112.5) fails" (expect-failure
  (exact 112.5)))

(define-test "(inexact)" (expect-success
  (assert-equal 567.0 (inexact 567))
  (assert-equal -3289.5 (inexact -3289.5))
  ; This is the closest float to the passed value
  (assert-equal 9007199254740992.0 (inexact 9007199254740993))))

(define-test "(=)" (expect-success
  (define dynamic-nan (typed-dynamic +nan.0 <flonum>))

  (assert-true  (= 4.0 4))
  (assert-true  (= 0.0 -0.0))
  (assert-true  (= 4.0 4 4.0))
  (assert-false (= 4.0 5.6))
  (assert-false (= 4.0 4 5.6))
  (assert-false (= +nan.0 +nan.0))
  (assert-false (= dynamic-nan +nan.0))
  (assert-false (= dynamic-nan 0))))

(define-test "equality of two numbers and boolean false is an error" (expect-failure
  (= 4.0 4 #f)))

(define-test "(<)" (expect-success
  (define dynamic-nan (typed-dynamic +nan.0 <flonum>))

  (assert-false (< 4.0 4))
  (assert-false (< -0.0 0.0))
  (assert-false (< 4.0 4 4.0))
  (assert-false (< 5.6 4.0))
  (assert-false (< 5.6 0 -4.5))
  (assert-true  (< 4.0 5.6))
  (assert-true  (< 4.0 4.5 5.6))
  (assert-false (< +nan.0 +nan.0))
  (assert-false (< dynamic-nan +nan.0))
  (assert-false (< dynamic-nan 0))))

(define-test "(>)" (expect-success
  (define dynamic-nan (typed-dynamic +nan.0 <flonum>))

  (assert-false (> 4.0 4))
  (assert-false (> -0.0 0.0))
  (assert-false (> 4.0 4 4.0))
  (assert-true  (> 5.6 4.0))
  (assert-true  (> 5.6 0 -4.5))
  (assert-false (> 4.0 5.6))
  (assert-false (> 4.0 4.5 5.6))
  (assert-false (> +nan.0 +nan.0))
  (assert-false (> dynamic-nan +nan.0))
  (assert-false (> dynamic-nan 0))))

(define-test "(<=)" (expect-success
  (define dynamic-nan (typed-dynamic +nan.0 <flonum>))

  (assert-true  (<= 4.0 4))
  (assert-true  (<= -0.0 0.0))
  (assert-true  (<= 4.0 4 4.0))
  (assert-false (<= 5.6 4.0))
  (assert-false (<= 5.6 0 -4.5))
  (assert-true  (<= 4.0 5.6))
  (assert-true  (<= 4.0 4.5 5.6))
  (assert-false (<= +nan.0 +nan.0))
  (assert-false (<= dynamic-nan +nan.0))
  (assert-false (<= dynamic-nan 0))))

(define-test "(>=)" (expect-success
  (define dynamic-nan (typed-dynamic +nan.0 <flonum>))

  (assert-true  (>= 4.0 4))
  (assert-true  (>= -0.0 0.0))
  (assert-true  (>= 4.0 4 4.0))
  (assert-true  (>= 5.6 4.0))
  (assert-true  (>= 5.6 0 -4.5))
  (assert-false (>= 4.0 5.6))
  (assert-false (>= 4.0 4.5 5.6))
  (assert-false (>= +nan.0 +nan.0))
  (assert-false (>= dynamic-nan +nan.0))
  (assert-false (>= dynamic-nan 0))))

(define-test "integer (<=) procedure" (expect-success
  (import (llambda typed))

  ; This is testing that we correctly deal with generating code that does multiple value comparisons
  ; The type annotations make it eligible for native code generation
  (: in-range (-> <exact-integer> <exact-integer> <exact-integer> <boolean>))
  (define (in-range lower val upper)
    (<= lower val upper))

  ; Strip the type to make sure we don't inline
  (define typeless-in-range (typeless-cell in-range))

  (assert-true (typeless-in-range 0 5 10))
  (assert-true (typeless-in-range 5 5 10))
  (assert-false (typeless-in-range 5 -5 10))
  (assert-false (typeless-in-range 5 15 10))

  (: byte-in-range (-> <exact-integer> <exact-integer> <exact-integer> <boolean>))
  (define (byte-in-range lower val upper)
    (<= 0 lower val upper 255))

  ; Strip the type to make sure we don't inline
  (define typeless-byte-in-range (typeless-cell byte-in-range))

  (assert-true (typeless-byte-in-range 0 5 10))
  (assert-true (typeless-byte-in-range 5 5 10))
  (assert-false (typeless-byte-in-range 5 11 10))
  ; This is inside the range we specify but outside of the byte range
  (assert-false (typeless-byte-in-range 0 300 500))))

(define-test "(zero?)" (expect-success
  (assert-true  (zero? 0))
  (assert-true  (zero? 0.0))
  (assert-false (zero? 34))
  (assert-false (zero? -134.5))
  (assert-false (zero? +inf.0))
  (assert-false (zero? -inf.0))
  (assert-false (zero? +nan.0))))

(define-test "(even?)" (expect-success
  (assert-true  (even? 1024))
  (assert-false (even? 777))
  (assert-true  (even? 0))
  (assert-true  (even? -1024))
  (assert-false (even? -777))))

(define-test "(odd?)" (expect-success
  (assert-false (odd? 1024))
  (assert-true  (odd? 777))
  (assert-false (odd? 0))
  (assert-false (odd? -1024))
  (assert-true  (odd? -777))))

(define-test "(positive?)" (expect-success
  (assert-false (positive? +nan.0))
  (assert-false (positive? 0))
  (assert-false (positive? 0.0))
  (assert-false (positive? -0.0))
  (assert-true  (positive? +inf.0))
  (assert-false (positive? -inf.0))
  (assert-true  (positive? 35))
  (assert-true  (positive? 456.7))
  (assert-false (positive? -35))
  (assert-false (positive? -456.7))))

(define-test "(negative?)" (expect-success
  (assert-false (negative? +nan.0))
  (assert-false (negative? 0))
  (assert-false (negative? 0.0))
  (assert-false (negative? -0.0))
  (assert-false (negative? +inf.0))
  (assert-true  (negative? -inf.0))
  (assert-false (negative? 35))
  (assert-false (negative? 456.7))
  (assert-true  (negative? -35))
  (assert-true  (negative? -456.7))))

(define-test "(max)" (expect-success
  (assert-equal -1 (max -1))
  (assert-equal 3 (max -1 2 3 2))
  (assert-equal 3.0 (max -1 2.5 3 2))
  (assert-equal 3.7 (max -1 2.5 3.7 2))))

(define-test "(min)" (expect-success
  (assert-equal -1 (min -1))
  (assert-equal -2 (min -1 2 3 -2))
  (assert-equal -3.0 (min -1 2.5 -3 2))
  (assert-equal -1.75 (min -1.75 2.5 3.7 2))))
