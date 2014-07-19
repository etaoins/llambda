(cond-expand (r7rs
  (define-test "(features) returns r7rs" (expect #t
    (if (memv 'r7rs (features)) #t #f)))))

(cond-expand (immutable-pairs
  (define-test "(features) returns immutable-pairs" (expect #t
    (if (memv 'immutable-pairs (features)) #t #f)))))

(define-test "(features) returns llambda" (expect #t
	(if (memv 'llambda (features)) #t #f))) 
