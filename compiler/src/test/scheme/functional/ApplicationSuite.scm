(define-test "application with insufficent args fails" (expect-failure
	(boolean?)))

(define-test "application with extraneous args fails" (expect-failure
	(boolean? #t #f)))
