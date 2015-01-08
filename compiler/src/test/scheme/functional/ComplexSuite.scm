(define-test "(make-rectangular)" (expect-success
  (import (llambda complex-stub))

  (assert-equal 5 (make-rectangular 5 0))
  (assert-equal -5 (make-rectangular -5 0))
  (assert-equal 5.0 (make-rectangular 5.0 0))
  (assert-equal -5.0 (make-rectangular -5.0 0))

  (assert-equal 0.0 (make-rectangular 0.0 0))
  (assert-equal -0.0 (make-rectangular -0.0 0))

  (assert-equal +inf.0 (make-rectangular +inf.0 0))
  (assert-equal -inf.0 (make-rectangular -inf.0 0))

  (assert-equal +nan.0 (make-rectangular +nan.0 0))))

(define-test "(make-rectangular) with imaginary part fails" (expect-error implementation-restriction-error?
  (import (llambda complex-stub))
  (make-rectangular 10.0 50.0)))

(define-test "(make-polar)" (expect-success
  (import (llambda complex-stub))

  (assert-equal 5 (make-polar 5 0))
  (assert-equal -5 (make-polar -5 0))
  (assert-equal 5.0 (make-polar 5.0 0))
  (assert-equal -5.0 (make-polar -5.0 0))

  (assert-equal 0.0 (make-polar 0.0 0))
  (assert-equal -0.0 (make-polar -0.0 0))

  (assert-equal +inf.0 (make-polar +inf.0 0))
  (assert-equal -inf.0 (make-polar -inf.0 0))

  (assert-equal +nan.0 (make-polar +nan.0 0))))

(define-test "(make-polar) with imaginary part fails" (expect-error implementation-restriction-error?
  (import (llambda complex-stub))
  (make-polar 10.0 5.0)))

(define-test "(real-part)" (expect-success
  (import (llambda complex-stub))

  (assert-equal 5 (real-part 5))
  (assert-equal -5 (real-part -5))
  (assert-equal 5.0 (real-part 5.0))
  (assert-equal -5.0 (real-part -5.0))

  (assert-equal 0.0 (real-part 0.0))
  (assert-equal -0.0 (real-part -0.0))

  (assert-equal +inf.0 (real-part +inf.0))
  (assert-equal -inf.0 (real-part -inf.0))

  (assert-equal +nan.0 (real-part +nan.0))))

(define-test "(imag-part)" (expect-success
  (import (llambda complex-stub))

  (assert-equal 0 (imag-part 5))
  (assert-equal 0 (imag-part -5))
  (assert-equal 0 (imag-part 5.0))
  (assert-equal 0 (imag-part -5.0))

  (assert-equal 0 (imag-part 0.0))
  (assert-equal 0 (imag-part -0.0))

  (assert-equal 0 (imag-part +inf.0))
  (assert-equal 0 (imag-part -inf.0))

  (assert-equal +nan.0 (imag-part +nan.0))))

(define-test "(magnitude)" (expect-success
  (import (llambda complex-stub))

  (assert-equal 5 (magnitude 5))
  (assert-equal 5 (magnitude -5))
  (assert-equal 5.0 (magnitude 5.0))
  (assert-equal 5.0 (magnitude -5.0))

  (assert-equal 0.0 (magnitude 0.0))
  (assert-equal 0.0 (magnitude -0.0))

  (assert-equal +inf.0 (magnitude +inf.0))
  (assert-equal +inf.0 (magnitude -inf.0))

  (assert-equal +nan.0 (magnitude +nan.0))))

(define-test "(angle)" (expect-success
  (import (llambda complex-stub))

  (define pi 3.14159)

  (assert-equal 0 (angle 5))
  (assert-within pi 0.001 (angle -5))
  (assert-equal 0 (angle 5.0))
  (assert-within pi 0.001 (angle -5.0))

  ; (angle) is undefined for zero
  (assert-raises error-object? (angle 0.0))
  (assert-raises error-object? (angle -0.0))

  (assert-equal 0 (angle +inf.0))
  (assert-within pi 0.001 (angle -inf.0))

  (assert-equal +nan.0 (angle +nan.0))))

(define-test "(angle) of zero fails" (expect-error invalid-argument-error?
  (import (llambda complex-stub))

  (angle 0)))

