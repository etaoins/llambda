; Super ghetto but anything else depends too much on floating point
; representations
(define-test "inexact trigonometric procedures" (expect-success
  (import (scheme inexact))
  (define pi 3.141592653589793)

  (assert-equal 0.0 (sin 0.0))
  (assert-within 1.0 0.001 (sin (/ pi 2)))

  (assert-equal 1.0 (cos 0.0))
  (assert-within 0.0 0.001 (cos (/ pi 2)))

  (assert-equal 0.0 (tan 0.0))
  (assert-true (> (tan (/ pi 2)) 10000))

  (assert-equal 0.0 (asin 0.0))
  (assert-within (/ pi 2) 0.001 (asin 1.0))

  (assert-within (/ pi 2) 0.001 (acos 0.0))
  (assert-equal 0.0 (acos 1.0))

  (assert-equal 0.0 (atan 0.0))
  (assert-within (/ pi 2) 0.001 (atan 1000000000))))

(define-test "exact trigonometric procedures" (expect-success
  (import (scheme inexact))
  (assert-equal 0.0 (sin 0))
  (assert-equal 1.0 (cos 0))
  (assert-equal 0.0 (tan 0))))

(define-test "(sqrt)" (expect-success
  (import (scheme inexact))
  (assert-equal 3.0 (sqrt 9))
  (assert-equal 3.0 (sqrt 9.0))
  (assert-equal 0.5 (sqrt 0.25))
  ; We don't support complex numbers
  (assert-equal +nan.0 (sqrt -9.0))))

(define-test "(finite?)" (expect-success
  (import (scheme inexact))
  (assert-true  (finite? 3))
  (assert-true  (finite? 4.5))
  (assert-false (finite? +inf.0))
  (assert-false (finite? +nan.0))))

(define-test "(infinite?)" (expect-success
  (import (scheme inexact))
  (assert-false (infinite? 3))
  (assert-false (infinite? 4.5))
  (assert-true  (infinite? +inf.0))
  (assert-false (infinite? +nan.0))))

(define-test "(nan?)" (expect-success
  (import (scheme inexact))
  (assert-false (nan? 3))
  (assert-false (nan? 4.5))
  (assert-false (nan? +inf.0))
  (assert-true  (nan? +nan.0))))

(define-test "(log)" (expect-success
  (import (scheme inexact))
  (assert-equal -inf.0 (log 0.0))
  (assert-within -0.693 0.01 (log 0.5))
  (assert-within 1.61 0.01 (log 5))
  (assert-within 3.91 0.01 (log 50))
  (assert-within 6.21 0.01 (log 500))))

(define-test "(exp)" (expect-success
  (import (scheme inexact))
  (assert-equal 1.0 (exp 0.0))
  (assert-within 2.72 0.01 (exp 1.0))
  (assert-within 148.41 0.01 (exp 5.0))))
