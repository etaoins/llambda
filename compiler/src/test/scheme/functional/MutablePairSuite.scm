(define-test "simple mutable pairs" (expect-success
  (import (llambda mpair))

  (define test-mpair (mcons 1 2))

  ; Pairs and mutable pairs are completely disjoint
  (assert-true (mpair? test-mpair))
  (assert-false (pair? test-mpair))

  (assert-equal 1 (mcar test-mpair))
  (assert-equal 2 (mcdr test-mpair))
  
  (set-mcar! test-mpair 'left)
  (set-mcdr! test-mpair 'right)
  
  (assert-equal 'left (mcar test-mpair))
  (assert-equal 'right (mcdr test-mpair))))
