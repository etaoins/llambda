(define-test "simple (case-lambda)" (expect ((0 1 2) . (3 4))
   (import (scheme case-lambda))
   (define range
     (case-lambda
       ((e) (range 0 e))
       ((b e) (do ((r '() (cons e r))
                   (e (- e 1) (- e 1)))
                ((< e b) r)))))
   (cons (range 3) (range 3 5))))

(define-test "(case-lambda) with rest args" (expect (2 3 4)
    (import (scheme case-lambda))
    (define rest-lambda
      (case-lambda
        ((first) 'first)
        ((first second) 'second)
        ((first second . rest) rest)))
    (rest-lambda 0 1 2 3 4)))

(define-test "(case-lambda) with wrong arity fails at compile time" (expect-compile-failure
    (import (scheme case-lambda))
    (define fixed-lambda
      (case-lambda
        ((first) 'first)
        ((first second) 'second)))
    (fixed-lambda 0 1 2)))
