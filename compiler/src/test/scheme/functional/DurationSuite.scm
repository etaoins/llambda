(define-test "(microseconds)" (expect-static-success
  (import (llambda duration))

  (assert-equal (microseconds 1) (microseconds 1))
  (assert-equal (microseconds 1) (microseconds 1.0))
  (assert-equal (microseconds 0) (microseconds 0.0))))

(define-test "(milliseconds)" (expect-static-success
  (import (llambda duration))

  (assert-equal (microseconds 1000) (milliseconds 1))
  (assert-equal (microseconds 1500) (milliseconds 1.5))
  (assert-equal (microseconds 25000) (milliseconds 25))
  (assert-equal (microseconds 5000000) (milliseconds 5000))))

(define-test "(seconds)" (expect-static-success
  (import (llambda duration))

  (assert-equal (microseconds 1000000) (seconds 1))
  (assert-equal (microseconds 1500000) (seconds 1.5))
  (assert-equal (microseconds 25000000) (seconds 25))
  (assert-equal (microseconds 5000000000) (seconds 5000))))

(define-test "(minutes)" (expect-static-success
  (import (llambda duration))

  (assert-equal (microseconds 60000000) (minutes 1))
  (assert-equal (microseconds 90000000) (minutes 1.5))
  (assert-equal (microseconds 1500000000) (minutes 25))
  (assert-equal (microseconds 300000000000) (minutes 5000))))

(define-test "(hours)" (expect-static-success
  (import (llambda duration))

  (assert-equal (microseconds 3600000000) (hours 1))
  (assert-equal (microseconds 5400000000) (hours 1.5))
  (assert-equal (microseconds 90000000000) (hours 25))
  (assert-equal (microseconds 18000000000000) (hours 5000))))
