(define-library (llambda complex-stub)
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))
  (import (scheme base))
  (import (only (scheme inexact) acos))
  (import (llambda typed))

  (export make-rectangular make-polar real-part imag-part magnitude angle)

  (begin
    (define-r7rs make-rectangular (lambda ([real : <number>] [imag : <number>])
      (unless (zero? imag)
        (error "Complex numbers are not supported"))
      real))

    (define-r7rs make-polar (lambda ([mag : <number>] [ang : <number>])
      (unless (zero? ang)
        (error "Complex numbers are not supported"))
      mag))

    (define-r7rs real-part (lambda ([num : <number>])
      num))

    (define-r7rs imag-part (lambda ([num : <number>])
      (if (equal? num +nan.0) +nan.0 0)))

    (define-r7rs magnitude (lambda ([num : <number>])
      (abs num)))

    (define-r7rs angle (lambda ([num : <number>])
      (cond
        ((equal? num +nan.0) +nan.0)
        ((zero? num) (error "(angle) is undefined for 0"))
        ((positive? num) 0)
        ; pi
        (else (* 2 (acos 0))))))))
