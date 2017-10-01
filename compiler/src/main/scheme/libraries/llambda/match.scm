(define-library (llambda match)
  (import (llambda internal primitives))
  (export match match-lambda)

  (begin
    (define-syntax match-lambda
      (syntax-rules ()
                    ((match-lambda clause ...)
                     (lambda (val)
                       (match val
                              clause ...)))))))
