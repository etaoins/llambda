(define-test "application with insufficent args fails" (expect-failure
	(import (scheme core))
	(boolean?)))

(define-test "application with extraneous args fails" (expect-failure
	(import (scheme core))
	(boolean? #t #f)))
