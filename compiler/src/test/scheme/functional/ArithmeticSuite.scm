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
  (assert-equal -64.0 (/ 128.0 -0.25 8))
  (assert-equal 1 (/ 1))
  (assert-equal -1 (/ -1))
  (assert-equal +inf.0 (/ 0))
  (assert-equal +inf.0 (/ 5 0))
  (assert-equal -inf.0 (/ -5 0))
  (assert-equal +nan.0 (/ -5 +nan.0))
  (assert-equal 0.5 (/ 20 5 2 4))

  ; This divides exactly
  (assert-equal 2 (/ 20 5 2))

  ; This divides exactly but contains an inexact value
  (assert-equal 2.0 (/ 20 5.0 2))))

(define-test "dividing single string fails" (expect-failure
  (/ "Hello!")))

(define-test "dividing no numbers fails" (expect-failure
  (/)))

(define-test "(truncate/)" (expect-success
  (let-values (((quot remain) (truncate/ 5 2)))
    (assert-equal 2 quot)
    (assert-equal 1 remain))

  (let-values (((quot remain) (truncate/ -5 2)))
    (assert-equal -2 quot)
    (assert-equal -1 remain))

  (let-values (((quot remain) (truncate/ 5 -2)))
    (assert-equal -2 quot)
    (assert-equal 1 remain))

  (let-values (((quot remain) (truncate/ -5 -2)))
    (assert-equal 2 quot)
    (assert-equal -1 remain))

  ; Our native code generation requires a constant denominator to avoid divide by zero checks
  (let-values (((quot remain) (truncate/ (typed-dynamic 5 <exact-integer>) 2)))
    (assert-equal 2 quot)
    (assert-equal 1 remain))

  (let-values (((quot remain) (truncate/ (typed-dynamic -5 <exact-integer>) 2)))
    (assert-equal -2 quot)
    (assert-equal -1 remain))

  (let-values (((quot remain) (truncate/ (typed-dynamic 5 <exact-integer>) -2)))
    (assert-equal -2 quot)
    (assert-equal 1 remain))

  (let-values (((quot remain) (truncate/ (typed-dynamic -5 <exact-integer>) -2)))
    (assert-equal 2 quot)
    (assert-equal -1 remain))))

(define-test "(truncate/) by zero fails" (expect-failure
  (truncate/ 5 0)))

(define-test "(truncate-quotient)" (expect-success
    (assert-equal 2 (truncate-quotient 5 2))
    (assert-equal -2 (truncate-quotient -5 2))
    (assert-equal -2 (truncate-quotient 5 -2))
    (assert-equal 2 (truncate-quotient -5 -2))

    (assert-equal 2 (truncate-quotient (typed-dynamic 5 <exact-integer>) 2))
    (assert-equal -2 (truncate-quotient (typed-dynamic -5 <exact-integer>) 2))
    (assert-equal -2 (truncate-quotient (typed-dynamic 5 <exact-integer>) -2))
    (assert-equal 2 (truncate-quotient (typed-dynamic -5 <exact-integer>) -2))))

(define-test "(truncate-quotient) by zero fails" (expect-failure
    (truncate-quotient 5 0)))

(define-test "(truncate-remainder)" (expect-success
    (assert-equal 1 (truncate-remainder 5 2))
    (assert-equal -1 (truncate-remainder -5 2))
    (assert-equal 1 (truncate-remainder 5 -2))
    (assert-equal -1 (truncate-remainder -5 -2))

    (assert-equal 1 (truncate-remainder (typed-dynamic 5 <exact-integer>) 2))
    (assert-equal -1 (truncate-remainder (typed-dynamic -5 <exact-integer>) 2))
    (assert-equal 1 (truncate-remainder (typed-dynamic 5 <exact-integer>) -2))
    (assert-equal -1 (truncate-remainder (typed-dynamic -5 <exact-integer>) -2))))

(define-test "(floor/)" (expect-success
  (let-values (((quot remain) (floor/ 5 2)))
    (assert-equal 2 quot)
    (assert-equal 1 remain))

  (let-values (((quot remain) (floor/ -5 2)))
    (assert-equal -3 quot)
    (assert-equal 1 remain))

  (let-values (((quot remain) (floor/ 5 -2)))
    (assert-equal -3 quot)
    (assert-equal -1 remain))

  (let-values (((quot remain) (floor/ -5 -2)))
    (assert-equal 2 quot)
    (assert-equal -1 remain))))

(define-test "(floor/) by zero fails" (expect-failure
  (floor/ 5 0)))

(define-test "(floor-quotient)" (expect-success
    (assert-equal 2 (floor-quotient 5 2))
    (assert-equal -3 (floor-quotient -5 2))
    (assert-equal -3 (floor-quotient 5 -2))
    (assert-equal 2 (floor-quotient -5 -2))))

(define-test "(floor-quotient) by zero fails" (expect-failure
    (floor-quotient 5 0)))

(define-test "(floor-remainder)" (expect-success
    (assert-equal 1 (floor-remainder 5 2))
    (assert-equal 1 (floor-remainder -5 2))
    (assert-equal -1 (floor-remainder 5 -2))
    (assert-equal -1 (floor-remainder -5 -2))))

; These are legacy aliases
(define-test "(quotient)" (expect 5
  (quotient 10 2)))

(define-test "(remainder)" (expect 0
  (remainder 10 2)))

(define-test "(modulo)" (expect 0
  (remainder 10 2)))

(define-test "(truncate-remainder) by zero fails" (expect-failure
    (truncate-remainder 5 0)))

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

(define-test "(gcd)" (expect-success
  (assert-equal 0 (gcd))
  (assert-equal 5 (gcd 5))
  (assert-equal 5 (gcd -5))
  (assert-equal 4 (gcd 32 -36))
  (assert-equal 2 (gcd 32 -36 202))))

(define-test "(lcm)" (expect-success
  (assert-equal 1 (lcm))
  (assert-equal 5 (lcm 5))
  (assert-equal 5 (lcm -5))
  (assert-equal 288 (lcm 32 -36))
  (assert-equal 576 (lcm 32 -36 192))))

(define-test "(exact-integer-sqrt)" (expect-success
  (let-values (((root rem) (exact-integer-sqrt 0)))
    (assert-equal 0 root)
    (assert-equal 0 rem))

  (let-values (((root rem) (exact-integer-sqrt 4)))
    (assert-equal 2 root)
    (assert-equal 0 rem))

  (let-values (((root rem) (exact-integer-sqrt 5)))
    (assert-equal 2 root)
    (assert-equal 1 rem))

  ; Ensure large integer values work correctly - converting to double won't work here
  (let-values (((root rem) (exact-integer-sqrt 4611686018427387911)))
    (assert-equal 2147483648 root)
    (assert-equal 7 rem))))

(define-test "(exact-integer-sqrt) with negative values fails" (expect-failure
  (exact-integer-sqrt -1)))

(define-test "(numerator), (denominator)" (expect-success
  (assert-equal 0 (numerator 0))
  (assert-equal 1 (denominator 0))

  (assert-equal 9223372036854775807 (numerator 9223372036854775807))
  (assert-equal 1 (denominator 9223372036854775807))

  (assert-equal -9223372036854775808 (numerator -9223372036854775808))
  (assert-equal 1 (denominator -9223372036854775808))

  (assert-equal 0.0 (numerator 0.0))
  (assert-equal 1.0 (denominator 0.0))

  (assert-equal -0.0 (numerator -0.0))
  (assert-equal 1.0 (denominator -0.0))

  (assert-equal +inf.0 (numerator +inf.0))
  (assert-equal 1.0 (denominator +inf.0))

  (assert-equal -inf.0 (numerator -inf.0))
  (assert-equal 1.0 (denominator -inf.0))

  (assert-equal +nan.0 (numerator +nan.0))
  (assert-equal +nan.0 (denominator +nan.0))

  (assert-equal 9223372036854775807.0 (numerator 9223372036854775807.0))
  (assert-equal 1.0 (denominator 9223372036854775807.0))

  (assert-equal -9223372036854775808.0 (numerator -9223372036854775808.0))
  (assert-equal 1.0 (denominator -9223372036854775808.0))

  (assert-equal 3.0 (numerator (/ 6.0 4)))
  (assert-equal 2.0 (denominator (/ 6.0 4)))

  (assert-equal 1.0 (numerator (/ 1.0 1024.0)))
  (assert-equal 1024.0 (denominator (/ 1.0 1024.0)))))
