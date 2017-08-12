(define-test "static (number?)" (expect-static-success
  (assert-true  (number? 4))
  (assert-true  (number? -5.0))
  (assert-false (number? '()))))

(define-test "dynamic (number?)" (expect-success
  (assert-true  (number? (typeless-cell 4)))
  (assert-true  (number? (typeless-cell -5.0)))
  (assert-false (number? (typeless-cell '())))))

(define-test "(rational?)" (expect-static-success
  (assert-true  (rational? 4))
  (assert-true  (rational? -5.0))
  (assert-false (rational? +inf.0))
  (assert-false (rational? +nan.0))
  (assert-false (rational? '()))))

(define-test "(integer?)" (expect-static-success
  (assert-false (integer? 3.0))
  (assert-true  (integer? 3.))
  (assert-false (integer? 'notanumber))))

(define-test "(flonum?)" (expect-static-success
  (assert-true  (flonum? 3.0))
  (assert-false (flonum? 3.))
  (assert-false (flonum? 32))
  (assert-false (flonum? 'notanumber))))

(define-test "(integer)" (expect-static-success
  (assert-equal -32 (integer -32.0))
  (assert-equal 64 (integer 64))))

(define-test "static (integer 112.5) fails" (expect-error invalid-argument-error?
  (integer 112.5)))

(define-test "dynamic (integer 112.5) fails" (expect-error invalid-argument-error?
  (integer (typed-dynamic 112.5 <flonum>))))

(define-test "static (flonum)" (expect-static-success
  (assert-equal 567.0 (flonum 567))
  (assert-equal -3289.5 (flonum -3289.5))
  ; This is the closest float to the passed value
  (assert-equal 9007199254740992.0 (flonum 9007199254740993))))

(define-test "dynamic (flonum)" (expect-success
  (assert-equal 567.0 (flonum (typed-dynamic 567 <integer>)))
  (assert-equal -3289.5 (flonum (typed-dynamic -3289.5 <flonum>)))
  ; This is the closest float to the passed value
  (assert-equal 9007199254740992.0 (flonum (typed-dynamic 9007199254740993 <integer>)))))

(define-test "static (=)" (expect-static-success
  (assert-true  (= 4.0 4))
  (assert-true  (= 0.0 -0.0))
  (assert-true  (= 0.0 0))
  (assert-true  (= 0 -0.0))
  (assert-true  (= 4.0 4 4.0))
  (assert-false (= 4.0 5.6))
  (assert-false (= 4.0 4 5.6))
  (assert-false (= +nan.0 +nan.0))))

(define-test "dynamic (=)" (expect-success
  (define dynamic-nan (typed-dynamic +nan.0 <flonum>))

  (assert-false (= dynamic-nan +nan.0))
  (assert-false (= dynamic-nan 0))

  (define dynamic-minus-zero (typed-dynamic -0.0 <flonum>))
  (define zero-is-equal (= 0.0 dynamic-minus-zero))

  (assert-true zero-is-equal)

  (when zero-is-equal
    ; Make sure we didn't replace -0.0 with +0.0 when propagating constants in this branch
    (assert-equal dynamic-minus-zero -0.0))))

(define-test "equality of two numbers and boolean false is an error" (expect-error type-error?
  (= 4.0 4 #f)))

(define-test "static (<)" (expect-static-success
  (assert-false (< 4.0 4))
  (assert-false (< -0.0 0.0))
  (assert-false (< 4.0 4 4.0))
  (assert-false (< 5.6 4.0))
  (assert-false (< 5.6 0 -4.5))
  (assert-true  (< 4.0 5.6))
  (assert-true  (< 4.0 4.5 5.6))))

(define-test "dynamic (<)" (expect-success
  (define dynamic-nan (typed-dynamic +nan.0 <flonum>))

  (assert-false (< +nan.0 +nan.0))
  (assert-false (< dynamic-nan +nan.0))
  (assert-false (< dynamic-nan 0))))

(define-test "static (>)" (expect-static-success
  (assert-false (> 4.0 4))
  (assert-false (> -0.0 0.0))
  (assert-false (> 4.0 4 4.0))
  (assert-true  (> 5.6 4.0))
  (assert-true  (> 5.6 0 -4.5))
  (assert-false (> 4.0 5.6))
  (assert-false (> 4.0 4.5 5.6))
  (assert-false (> +nan.0 +nan.0))))

(define-test "dynamic (>)" (expect-success
  (define dynamic-nan (typed-dynamic +nan.0 <flonum>))

  (assert-false (> dynamic-nan +nan.0))
  (assert-false (> dynamic-nan 0))))

(define-test "static (<=)" (expect-static-success
  (assert-true  (<= 4.0 4))
  (assert-true  (<= -0.0 0.0))
  (assert-true  (<= 4.0 4 4.0))
  (assert-false (<= 5.6 4.0))
  (assert-false (<= 5.6 0 -4.5))
  (assert-true  (<= 4.0 5.6))
  (assert-true  (<= 4.0 4.5 5.6))
  (assert-false (<= +nan.0 +nan.0))))

(define-test "dynamic (<=)" (expect-success
  (define dynamic-nan (typed-dynamic +nan.0 <flonum>))

  (assert-false (<= dynamic-nan +nan.0))
  (assert-false (<= dynamic-nan 0))))

(define-test "static (>=)" (expect-static-success
  (assert-true  (>= 4.0 4))
  (assert-true  (>= -0.0 0.0))
  (assert-true  (>= 4.0 4 4.0))
  (assert-true  (>= 5.6 4.0))
  (assert-true  (>= 5.6 0 -4.5))
  (assert-false (>= 4.0 5.6))
  (assert-false (>= 4.0 4.5 5.6))
  (assert-false (>= +nan.0 +nan.0))))

(define-test "dynamic (>=)" (expect-success
  (define dynamic-nan (typed-dynamic +nan.0 <flonum>))

  (assert-false (>= dynamic-nan +nan.0))
  (assert-false (>= dynamic-nan 0))))

(define-test "integer (<=) procedure" (expect-success
  (import (llambda typed))

  ; This is testing that we correctly deal with generating code that does multiple value comparisons
  ; The type annotations make it eligible for native code generation
  (: in-range (-> <integer> <integer> <integer> <boolean>))
  (define (in-range lower val upper)
    (<= lower val upper))

  ; Strip the type to make sure we don't inline
  (define typeless-in-range (typeless-cell in-range))

  (assert-true (typeless-in-range 0 5 10))
  (assert-true (typeless-in-range 5 5 10))
  (assert-false (typeless-in-range 5 -5 10))
  (assert-false (typeless-in-range 5 15 10))

  (: byte-in-range (-> <integer> <integer> <integer> <boolean>))
  (define (byte-in-range lower val upper)
    (<= 0 lower val upper 255))

  ; Strip the type to make sure we don't inline
  (define typeless-byte-in-range (typeless-cell byte-in-range))

  (assert-true (typeless-byte-in-range 0 5 10))
  (assert-true (typeless-byte-in-range 5 5 10))
  (assert-false (typeless-byte-in-range 5 11 10))
  ; This is inside the range we specify but outside of the byte range
  (assert-false (typeless-byte-in-range 0 300 500))))

(define-test "(zero?)" (expect-static-success
  (assert-true  (zero? 0))
  (assert-true  (zero? 0.0))
  (assert-false (zero? 34))
  (assert-false (zero? -134.5))
  (assert-false (zero? +inf.0))
  (assert-false (zero? -inf.0))
  (assert-false (zero? +nan.0))))

(define-test "(even?)" (expect-static-success
  (assert-true  (even? 1024))
  (assert-false (even? 777))
  (assert-true  (even? 0))
  (assert-true  (even? -1024))
  (assert-false (even? -777))))

(define-test "(odd?)" (expect-static-success
  (assert-false (odd? 1024))
  (assert-true  (odd? 777))
  (assert-false (odd? 0))
  (assert-false (odd? -1024))
  (assert-true  (odd? -777))))

(define-test "(positive?)" (expect-static-success
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

(define-test "(negative?)" (expect-static-success
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

(define-test "(max)" (expect-static-success
  (assert-equal -1 (max -1))
  (assert-equal 3 (max -1 2 3 2))
  (assert-equal 3 (max -1 2.5 3 2))
  (assert-equal 3.7 (max -1 2.5 3.7 2))))

(define-test "(max) with non-numeric argument fails" (expect-compile-error type-error?
  (max #t)))

(define-test "(min)" (expect-static-success
  (assert-equal -1 (min -1))
  (assert-equal -2 (min -1 2 3 -2))
  (assert-equal -3 (min -1 2.5 -3 2))
  (assert-equal -1.75 (min -1.75 2.5 3.7 2))))

(define-test "(min) with non-numeric argument fails" (expect-compile-error type-error?
  (min #t)))
