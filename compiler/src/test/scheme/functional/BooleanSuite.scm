(define-test "static (boolean?)" (expect-static-success
  (assert-true  (boolean? #f))
  (assert-true  (boolean? #t))
  (assert-false (boolean? 0))
  (assert-false (boolean? '()))))

(define-test "dynamic (boolean?)" (expect-success
  (assert-true  (boolean? (typeless-cell #f)))
  (assert-true  (boolean? (typeless-cell #t)))
  (assert-false (boolean? (typeless-cell 0)))
  (assert-false (boolean? (typeless-cell '())))))

(define-test "(not)" (expect-static-success
  (assert-equal #f (not #t))
  (assert-equal #f (not 3))
  (assert-equal #f (not '(3)))
  (assert-equal #t (not #f))
  (assert-equal #f (not 'nil))
  (assert-equal #f (not +))))

(define-test "(boolean=?)" (expect-static-success
  (assert-equal #t (boolean=? #t #t))
  (assert-equal #t (boolean=? #t #t #t))
  (assert-equal #f (boolean=? #t #t #f))
  (assert-equal #t (boolean=? #f #f #f))))

(define-test "(boolean=?) requires at least two arguments" (expect-compile-error arity-error?
  (boolean=? #t)))

(define-test "(boolean=?) with non-booleans fails" (expect-compile-error type-error?
  (boolean=? 0 0)))

(define-test "(boolean=?) with two trues and one zero fails" (expect-compile-error type-error?
  (boolean=? #t #t 0)))
