(define-test "boxes are disjoint from pairs" (expect #f
  (import (srfi 111))
  (pair? (box #t))))

(define-test "box predicate" (expect #t
  (import (srfi 111))
  (box? (box #t))))

(define-test "directly unboxing without mutation" (expect value1
  (import (srfi 111))
  (unbox (box 'value1))))

(define-test "unboxing after mutation" (expect value2
  (import (srfi 111))
  (define test-box (box 'value1))
  (set-box! test-box 'value2)
  (unbox test-box)))
