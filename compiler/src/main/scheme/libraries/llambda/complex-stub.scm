(define-library (llambda complex-stub)
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))
  (import (scheme base))
  (import (only (scheme inexact) acos))
  (import (llambda typed))
  (import (llambda error))

  (export make-rectangular make-polar real-part imag-part magnitude angle)

  (begin
    (define-r7rs (make-rectangular [real : <number>] [imag : <number>])
      (unless (zero? imag)
        (raise-implementation-restriction-error "Complex numbers are not supported"))
      real)

    (define-r7rs (make-polar [mag : <number>] [ang : <number>])
      (cond
        ((equal? ang +nan.0) +nan.0)
        ((zero? ang) mag)
        ((= ang (* 2 (acos 0))) (- mag))
        (else
          (raise-implementation-restriction-error "Complex numbers are not supported"))))

    (define-r7rs (real-part [num : <number>])
      num)

    (define-r7rs (imag-part [num : <number>])
      (if (equal? num +nan.0) +nan.0 0))

    (define-r7rs (magnitude [num : <number>])
      (abs num))

    (define-r7rs (angle [num : <number>])
      (cond
        ((equal? num +nan.0) +nan.0)
        ((zero? num) (raise-invalid-argument-error "(angle) is undefined for 0"))
        ((positive? num) 0)
        ; pi
        (else (* 2 (acos 0)))))))
