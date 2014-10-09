; Super ghetto but anything else depends too much on floating point
; representations
(define-test "inexact trigonometric procedures" (expect-success
  (import (scheme inexact))
  (assert-equal 0.0 (sin 0.0))
  (assert-equal 1.0 (cos 0.0))
  (assert-equal 0.0 (tan 0.0))))

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
  
