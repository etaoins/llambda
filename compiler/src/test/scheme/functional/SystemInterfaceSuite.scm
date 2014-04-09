(define-test "(features) returns r7rs" (expect #t
	(if (memv 'r7rs (features)) #t #f))) 

(define-test "(features) returns llambda" (expect #t
	(if (memv 'llambda (features)) #t #f))) 
