(define-test "simple (case-lambda)" (expect ((0 1 2) . (3 4))
   (import (scheme case-lambda))
   (define range
     (case-lambda
       ((e) (range 0 e))
       ((b e) (do ((r '() (cons e r))
                   (e (- e 1) (- e 1)))
                ((< e b) r)))))
   (cons (range 3) (range 3 5))))
