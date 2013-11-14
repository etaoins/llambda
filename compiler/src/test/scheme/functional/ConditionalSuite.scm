(define-test "empty (and) evaluates to true" (expect #t
	(and)))

(define-test "(and #t #f) is false" (expect #f
	(and #t #f)))

(define-test "(and) returns the last evaluated datum" (expect (f g)
	(and 1 2 'c '(f g))))

(define-test "empty (or) evaluates to false" (expect #f
	(or)))

(define-test "(or #t #f) is true" (expect #t
	(or #t #f)))

(define-test "(or) returns the last evaluated datum" (expect (b c)
	(or #f '(b c) #t)))
