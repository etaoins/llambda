(define-library (llambda match)
  (import (llambda internal primitives))
  (export match match-lambda)

  (begin
    ; This is analogous to (case-lambda) except it matches a single passed value
    (define-syntax match-lambda
      (syntax-rules ()
                    ((match-lambda clause ...)
                     (lambda (val)
                       (match val
                              clause ...)))))))
