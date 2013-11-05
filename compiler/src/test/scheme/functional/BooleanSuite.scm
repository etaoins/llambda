(define-test "false is boolean" (expect #t
	(boolean? #f)))

(define-test "0 is not boolean" (expect #f
	(boolean? 0)))

(define-test "empty list is not boolean" (expect #f
	(boolean? '())))

(define-test "not true is false" (expect #f
	(not #t)))

(define-test "not 3 is false" (expect #f
	(not 3)))

(define-test "not (3) is false" (expect #f
	(not '(3))))

(define-test "not false is true" (expect #t
	(not #f)))

(define-test "not 'nil is false" (expect #f
	(not 'nil)))

(define-test "not + is false" (expect #f
	(not +)))

(define-test "boolean=? requires at least two arguments" (expect-failure
	(boolean=? #t)))

(define-test "boolean=? with non-booleans fails" (expect-failure
	(boolean=? 0 0)))

(define-test "boolean=? with two trues" (expect #t
	(boolean=? #t #t)))

(define-test "boolean=? with two trues and one zero fails" (expect-failure
	(boolean=? #t #t 0)))

(define-test "boolean=? with three trues" (expect #t
	(boolean=? #t #t #t)))

(define-test "boolean=? with two trues and one false" (expect #f
	(boolean=? #t #t #f)))

(define-test "boolean=? with three falses" (expect #t
	(boolean=? #f #f #f)))
