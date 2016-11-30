(define-test "(current-second)" (expect-success
  (import (scheme time))
  (define cur-second (current-second))

  (assert-true (flonum? cur-second))

  ; Make sure the returned time is in the 21st century
  (assert-true (> cur-second 946684800))
  (assert-true (< cur-second 4102444800))))

(define-test "(current-unix-time)" (expect-success
  (import (llambda time))
  (define cur-time (current-unix-time))

  (assert-true (flonum? cur-time))

  ; Make sure the returned time is in the 21st century
  (assert-true (> cur-time 946684800))
  (assert-true (< cur-time 4102444800))

  (assert-within (current-second) 200 (current-unix-time))))

(define-test "(current-jiffy), (jiffies-per-second)" (expect-success
  (import (scheme time))
  (define cur-jiffy (current-jiffy))
  (define jps (jiffies-per-second))

  (assert-true (integer? cur-jiffy))
  (assert-true (>= cur-jiffy 0))

  (assert-true (integer? jps))
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
