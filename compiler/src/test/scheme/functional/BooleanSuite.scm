(define-test "(boolean?)" (expect-success
  (assert-true  (boolean? #f))
  (assert-true  (boolean? (typeless-cell #f)))

  (assert-true  (boolean? #t))
  (assert-true  (boolean? (typeless-cell #t)))

  (assert-false (boolean? 0))
  (assert-false (boolean? (typeless-cell 0)))
  
  (assert-false (boolean? '()))
  (assert-false (boolean? (typeless-cell '())))))

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

(define-test "boolean=? requires at least two arguments" (expect-error arity-error?
	(boolean=? #t)))

(define-test "boolean=? with non-booleans fails" (expect-error type-error?
	(boolean=? 0 0)))

(define-test "boolean=? with two trues" (expect #t
	(boolean=? #t #t)))

(define-test "boolean=? with two trues and one zero fails" (expect-error type-error?
	(boolean=? #t #t 0)))

(define-test "boolean=? with three trues" (expect #t
	(boolean=? #t #t #t)))

(define-test "boolean=? with two trues and one false" (expect #f
	(boolean=? #t #t #f)))

(define-test "boolean=? with three falses" (expect #t
	(boolean=? #f #f #f)))
