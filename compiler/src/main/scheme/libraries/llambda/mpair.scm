(define-library (llambda mpair)
  (import (llambda internal primitives))

  (export <mpair> mpair? mcons mcar mcdr set-mcar! set-mcdr!)
  
  (begin
    (define-record-type <mpair> (mcons mcar mcdr) mpair?
      (mcar mcar set-mcar!)
      (mcdr mcdr set-mcdr!)))
)
