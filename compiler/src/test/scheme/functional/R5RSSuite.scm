(define-test "(inexact->exact)" (expect-success
  (import (scheme r5rs))
  (assert-equal -32 (inexact->exact -32.0))
  (assert-equal 64 (inexact->exact 64))))

(define-test "(exact->inexact)" (expect-success
  (import (scheme r5rs))
  (assert-equal 567.0 (exact->inexact 567))
  (assert-equal -3289.5 (exact->inexact -3289.5))))
