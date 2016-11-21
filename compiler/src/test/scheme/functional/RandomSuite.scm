(define-test "(random-integer)" (expect-success
  (import (llambda random))
  (import (llambda list))
  (import (scheme write))

  (assert-equal 0 (random-integer 1))

  (list-tabulate 25 (lambda (x)
                      (define value (random-integer (+ x 1)))

                      (assert-true (>= value 0))
                      (assert-true (<= value x))))))

(define-test "(random-integer) with zero fails" (expect-error range-error?
  (import (llambda random))

  (random-integer 0)))

(define-test "(random-integer) with negative number fails" (expect-error range-error?
  (import (llambda random))

  (random-integer -10)))

(define-test "(random-real)" (expect-success
  (import (llambda random))

  (for-each (lambda (_)
              (define value (random-real))

              (assert-true (> value 0))
              (assert-true (< value 1)))
            (make-list 25))))
