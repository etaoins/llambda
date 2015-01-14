(define-test "R5RS programs are case folded" (expect-success
  (Define MY-LIST (LIST 'ONE 'Two 'three))

  (assert-equal (car my-list) 'one)
  (ASSERT-EQUAL (cdr My-List) '(TWO Three))))

(define-test "(inexact->exact)" (expect-success
  (assert-equal -32 (inexact->exact -32.0))
  (assert-equal 64 (inexact->exact 64))))

(define-test "(exact->inexact)" (expect-success
  (assert-equal 567.0 (exact->inexact 567))
  (assert-equal -3289.5 (exact->inexact -3289.5))))
