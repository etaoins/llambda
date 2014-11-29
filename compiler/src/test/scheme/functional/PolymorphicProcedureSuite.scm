(cond-expand (immutable-pairs
  (define-test "(cons) is polymorphic" (expect-success
    (import (llambda typed))

    (define exact-1 (typed-dynamic 1 <exact-integer>))
    (define inexact-1 (typed-dynamic 1.0 <flonum>))

    (ann (cons exact-1 inexact-1) (Pairof <exact-integer> <flonum>))))

  (define-test "(car) is polymorphic" (expect-success
    (import (llambda typed))

    (define exact-1 (typed-dynamic 1 <exact-integer>))
    (define inexact-1 (typed-dynamic 1.0 <flonum>))

    (ann (car (cons exact-1 inexact-1)) <exact-integer>)))

  (define-test "(cdr) is polymorphic" (expect-success
    (import (llambda typed))

    (define exact-1 (typed-dynamic 1 <exact-integer>))
    (define inexact-1 (typed-dynamic 1.0 <flonum>))

    (ann (cdr (cons exact-1 inexact-1)) <flonum>)))

  (define-test "(memv) is polymorphic" (expect-success
    (import (llambda typed))

    (define exact-list (typed-dynamic '(1 2 4) (Listof <exact-integer>)))
    (ann (memv 2 exact-list) (U #f (Listof <exact-integer>)))))

  (define-test "(member) is polymorphic" (expect-success
    (import (llambda typed))

    (define exact-list (typed-dynamic '(1 2 4) (Listof <exact-integer>)))
    (ann (member 2 exact-list) (U #f (Listof <exact-integer>)))))

  (define-test "(assv) is polymorphic" (expect-success
    (import (llambda typed))

    (define symbol-to-int (typed-dynamic '((one . 1) (two . 2) (four . 4)) (Listof (Pairof <symbol> <exact-integer>))))

    (ann (assv 'one symbol-to-int) (U #f (Pairof <symbol> <exact-integer>)))))

  (define-test "(reverse) is polymorphic" (expect-success
    (import (llambda typed))

    (define integer-list (typed-dynamic '(1 2 3) (Listof <exact-integer>)))
    (define reverse-list (reverse integer-list))

    (ann integer-list (Listof <exact-integer>))))))

(define-test "(+) is polymorphic" (expect-success
  (import (llambda typed))

  (define exact-1 (typed-dynamic 1 <exact-integer>))
  (define exact-2 (typed-dynamic 1 <exact-integer>))

  (define inexact-1 (typed-dynamic 1.0 <flonum>))
  (define inexact-2 (typed-dynamic 2.0 <flonum>))

  (ann (+ exact-1 exact-2) <exact-integer>)
  (ann (+ inexact-1 inexact-2) <flonum>)))

(define-test "(-) is polymorphic" (expect-success
  (import (llambda typed))

  (define exact-1 (typed-dynamic 1 <exact-integer>))
  (define exact-2 (typed-dynamic 1 <exact-integer>))

  (define inexact-1 (typed-dynamic 1.0 <flonum>))
  (define inexact-2 (typed-dynamic 2.0 <flonum>))

  (ann (- exact-1 exact-2) <exact-integer>)
  (ann (- inexact-1 inexact-2) <flonum>)))

(define-test "(*) is polymorphic" (expect-success
  (import (llambda typed))

  (define exact-1 (typed-dynamic 1 <exact-integer>))
  (define exact-2 (typed-dynamic 1 <exact-integer>))

  (define inexact-1 (typed-dynamic 1.0 <flonum>))
  (define inexact-2 (typed-dynamic 2.0 <flonum>))

  (ann (* exact-1 exact-2) <exact-integer>)
  (ann (* inexact-1 inexact-2) <flonum>)))

(define-test "(min) is polymorphic" (expect-success
  (import (llambda typed))

  (define exact-1 (typed-dynamic 1 <exact-integer>))
  (define exact-2 (typed-dynamic 1 <exact-integer>))

  (define inexact-1 (typed-dynamic 1.0 <flonum>))
  (define inexact-2 (typed-dynamic 2.0 <flonum>))

  (ann (min exact-1 exact-2) <exact-integer>)
  (ann (min inexact-1 inexact-2) <flonum>)))

(define-test "(max) is polymorphic" (expect-success
  (import (llambda typed))

  (define exact-1 (typed-dynamic 1 <exact-integer>))
  (define exact-2 (typed-dynamic 1 <exact-integer>))

  (define inexact-1 (typed-dynamic 1.0 <flonum>))
  (define inexact-2 (typed-dynamic 2.0 <flonum>))

  (ann (max exact-1 exact-2) <exact-integer>)
  (ann (max inexact-1 inexact-2) <flonum>)))
