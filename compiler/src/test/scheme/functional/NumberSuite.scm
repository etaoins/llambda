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

(define-test "(+)" (expect-success
  (assert-equal 0 (+))
  (assert-equal 12 (+ 12))
  (assert-equal -450.5 (+ -450.5))
  (assert-equal -435065 (+ 70 -1024589 589454))
  (assert-equal 300.0 (+ 100.5 -0.5 200.0))
  (assert-equal 300.0 (+ 100.5 -0.5 200))
             
  (define dynamic-5 (length (typeless-cell '(1 2 3 4 5))))
  (assert-equal 8 (+ dynamic-5 1 2))))

(define-test "adding single string fails" (expect-failure
  (+ "Hello!")))

(define-test "(*)" (expect-success
  (assert-equal 1 (*))
  (assert-equal 12 (* 12))
  (assert-equal -450.5 (* -450.5))
  (assert-equal -499332738025 (* 4135 -3547 34045))
  (assert-equal -10050.0 (* 100.5 -0.5 200.0))
  (assert-equal 10050.0 (* 100.5 0.5 200))
  
  (define dynamic-5 (length (typeless-cell '(1 2 3 4 5))))
  (assert-equal 10 (* dynamic-5 1 2))))

(define-test "multiplying single string fails" (expect-failure
  (* "Hello!")))

(define-test "(-)" (expect-success
  (assert-equal -12 (- 12))
  (assert-equal 450.5 (- -450.5))
  (assert-equal -26363 (- 4135 -3547 34045))
  (assert-equal -99.0 (- 100.5 -0.5 200.0))
  (assert-equal -100.0 (- 100.5 0.5 200))
  
  (define dynamic-5 (length (typeless-cell '(1 2 3 4 5))))
  (assert-equal 2 (- dynamic-5 1 2))
  (assert-equal -6 (- 1 2 dynamic-5))))

(define-test "subtracting no numbers fails" (expect-failure
  (-)))

(define-test "subtracting single string fails" (expect-failure
  (- "Hello!")))

(define-test "(/)" (expect-success
  (assert-equal 0.125 (/ 8))
  (assert-equal -4.0 (/ -0.25))
  (assert-equal 0.15 (/ 3 4 5))
  (assert-equal 64.0 (/ 128.0 0.25 8))
  (assert-equal -64.0 (/ 128.0 -0.25 8))))

(define-test "dividing single string fails" (expect-failure
  (/ "Hello!")))

(define-test "dividing no numbers fails" (expect-failure
  (/)))

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
  (assert-false (negative? +inf.0))
  (assert-true  (negative? -inf.0))
  (assert-false (negative? 35))
  (assert-false (negative? 456.7))
  (assert-true  (negative? -35))
  (assert-true  (negative? -456.7))))

(define-test "rounding procedures" (expect-success
  (assert-equal -5.0 (floor -4.3))
  (assert-equal -4.0 (ceiling -4.3))
  (assert-equal -4.0 (truncate -4.3))
  (assert-equal -4.0 (round -4.3))
  (assert-equal 3.0  (floor 3.5))
  (assert-equal 4.0  (ceiling 3.5))
  (assert-equal 3.0  (truncate 3.5))
  (assert-equal 4.0  (round 3.5))
  (assert-equal 7    (round 7))))

(define-test "(expt)" (expect-success
  ; This is exact and within the range we can represent
  (assert-true (eqv? (expt 2 16) 65536))
  
  ; These are inexact versions of the above
  (assert-true (eqv? (expt 2.0 16) 65536.0))
  (assert-true (eqv? (expt 2 16.0) 65536.0))
  (assert-true (eqv? (expt 2.0 16) 65536.0))

  ; Make sure non-integers work
  (assert-true (eqv? (expt 0.5 3) 0.125))

  ; This is within the range of values we can exactly represent on all platforms
  (assert-true (eqv? (expt 2 53) 9007199254740992))

  ; This is outside the range we can exactly represent
  (assert-true (eqv? (expt 2 63) 9223372036854775808.0))))

(define-test "typed procedure adding multiple number types" (expect-success
  (import (llambda typed))

  (: add-nums (-> <flonum> <flonum> <exact-integer> <flonum>))
  (define (add-nums op1 op2 op3)
    (+ op1 op2 op3))

  (assert-equal 83.5 (add-nums 100.5 -50.0 33))))

(define-test "typed procedure muliplying multiple number types" (expect-success
  (import (llambda typed))

  (: mul-nums (-> <flonum> <flonum> <exact-integer> <flonum>))
  (define (mul-nums op1 op2 op3)
    (* op1 op2 op3))

  (assert-equal -5.0 (mul-nums -0.25 0.5 40))))

(define-test "typed procedure subtracting multiple number types" (expect-success
  (import (llambda typed))

  (: sub-nums (-> <flonum> <flonum> <exact-integer> <flonum>))
  (define (sub-nums op1 op2 op3)
    (- op1 op2 op3))

  (assert-equal 0.0 (sub-nums 50.0 20.0 30))))

(define-test "typed procedure dividing multiple number types" (expect-success
  (import (llambda typed))

  (: div-nums (-> <flonum> <flonum> <exact-integer> <flonum>))
  (define (div-nums op1 op2 op3)
    (/ op1 op2 op3))

  (assert-equal -5.0 (div-nums 500.0 25.0 -4))))

(define-test "(square)" (expect-success
  (assert-equal 1764 (square 42))
  (assert-equal 4.0 (square 2.0))))

(define-test "(abs)" (expect-success
  (assert-equal 0 (abs 0))
  (assert-equal 0.0 (abs 0.0))
  (assert-equal 0.0 (abs -0.0))
  (assert-equal 7 (abs 7))
  (assert-equal 7 (abs -7))
  (assert-equal 7.0 (abs -7.0))
  (assert-equal +nan.0 (abs +nan.0))
  (assert-equal +inf.0 (abs +inf.0))
  (assert-equal +inf.0 (abs -inf.0))))
