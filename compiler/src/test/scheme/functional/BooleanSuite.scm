(define-test "(boolean?)" (expect-success
  (assert-true  (boolean? #f))
  (assert-true  (boolean? (typeless-cell #f)))

  (assert-true  (boolean? #t))
  (assert-true  (boolean? (typeless-cell #t)))

  (assert-false (boolean? 0))
  (assert-false (boolean? (typeless-cell 0)))

  (assert-false (boolean? '()))
  (assert-false (boolean? (typeless-cell '())))))

(define-test "(not)" (expect-success
	(assert-equal #f (not #t))
	(assert-equal #f (not 3))
	(assert-equal #f (not '(3)))
	(assert-equal #t (not #f))
	(assert-equal #f (not 'nil))
	(assert-equal #f (not +))))

(define-test "(boolean=?)" (expect-success
	(assert-equal #t (boolean=? #t #t))
	(assert-equal #t (boolean=? #t #t #t))
	(assert-equal #f (boolean=? #t #t #f))
	(assert-equal #t (boolean=? #f #f #f))))

(define-test "boolean=? requires at least two arguments" (expect-error arity-error?
	(boolean=? #t)))

(define-test "boolean=? with non-booleans fails" (expect-error type-error?
	(boolean=? 0 0)))

(define-test "boolean=? with two trues and one zero fails" (expect-error type-error?
	(boolean=? #t #t 0)))
