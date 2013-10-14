(define-test "false is boolean" (expect #t
	(import (scheme core))
	(boolean? #f)))

(define-test "0 is not boolean" (expect #f
	(import (scheme core))
	(boolean? 0)))

(define-test "empty list is not boolean" (expect #f
	(import (scheme core))
	(boolean? '())))

(define-test "not true is false" (expect #f
	(import (scheme core))
	(not #t)))

(define-test "not 3 is false" (expect #f
	(import (scheme core))
	(not 3)))

(define-test "not (3) is false" (expect #f
	(import (scheme core))
	(not '(3))))

(define-test "not false is true" (expect #t
	(import (scheme core))
	(not #f)))

(define-test "not 'nil is true" (expect #f
	(import (scheme core))
	(not 'nil)))
