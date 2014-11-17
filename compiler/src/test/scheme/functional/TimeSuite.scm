(define-test "(current-second)" (expect-success
  (import (scheme time))
  (define cur-second (current-second))

  (assert-true (inexact? cur-second))

  ; Make sure the returned time is in the 21st century
  (assert-true (> cur-second 946684800))
  (assert-true (< cur-second 4102444800))))

(define-test "(current-jiffy), (jiffies-per-second)" (expect-success
  (import (scheme time))
  (define cur-jiffy (current-jiffy))
  (define jps (jiffies-per-second))

  (assert-true (exact? cur-jiffy))
  (assert-true (>= cur-jiffy 0))

  (assert-true (exact? jps))
  (assert-true (> jps 0))))

(define-test "simple (current-jiffy) benchmarking" (expect-success
  (import (scheme time))

  (define (time-length)
    (let ((list (make-list 100000))
          (start (current-jiffy)))
      (length list)
      (/ (- (current-jiffy) start)
         (jiffies-per-second))))

  (assert-true (> (time-length) 0))))
