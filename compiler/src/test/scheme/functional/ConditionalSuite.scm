(define-test "empty (and) evaluates to true" (expect #t
	(import (scheme core))
	(and)))

(define-test "(and #t #f) is falso" (expect #f
	(import (scheme core))
	(and #t #f)))

(define-test "(and) returns the last evaluated datum" (expect (f g)
	(import (scheme core))
	(and 1 2 'c '(f g))))

(define-test "empty (or) evaluates to false" (expect #f
	(import (scheme core))
	(or)))

(define-test "(or #t #f) is true" (expect #t
	(import (scheme core))
	(or #t #f)))

(define-test "(or) returns the last evaluated datum" (expect (b c)
	(import (scheme core))
	(or #f '(b c) #t)))
