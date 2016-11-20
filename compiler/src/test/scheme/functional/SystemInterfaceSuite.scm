(define-test "(features) returns llambda" (expect #t
  (if (memv 'llambda (features)) #t #f)))
