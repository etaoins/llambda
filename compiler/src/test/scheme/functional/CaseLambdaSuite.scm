(define-test "simple (case-lambda)" (expect ((0 1 2) . (3 4))
 (import (scheme case-lambda))

 (define range
   (case-lambda
     ((e) (range 0 e))
     ((b e) (do ((r '() (cons e r))
                 (e (- e 1) (- e 1)))
              ((< e b) r)))))
 (cons (range 3) (range 3 5))))

(define-test "simple (case-lambda) using R7RS definition" (expect ((0 1 2) . (3 4))
  (import (llambda r7rs-case-lambda))
  (define range
    (r7rs-case-lambda
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

(define-test "(case-lambda) cell" (expect-success
  (import (scheme case-lambda))

  ; This ensures our generated top-level (case-lambda) function works correctly
  (define case-cell (typeless-cell (case-lambda
    ((one) (- one))
    ((one two) (* one two))
    ((one two three) (+ one two three))
    (rest rest))))

  (assert-equal -1 (case-cell 1))
  (assert-equal 2 (case-cell 1 2))
  (assert-equal 6 (case-cell 1 2 3))
  (assert-equal '(1 2 3 4) (case-cell 1 2 3 4))))

(define-test "(case-lambda) with wrong arity fails at compile time" (expect-compile-failure
  (import (scheme case-lambda))

  (define fixed-lambda
    (case-lambda
      ((first) 'first)
      ((first second) 'second)))
  (fixed-lambda 0 1 2)))

(define-test "(case-lambda:) with type fails at compile time" (expect-compile-failure
  (import (scheme case-lambda))
  (import (llambda typed))

  (define fixed-lambda
    (case-lambda:
      (((first : <exact-integer>)) 'first)
      (((first : <exact-integer>) (second : <symbol>)) 'second)))
  (fixed-lambda 0 1)))

(define-test "(case-lambda:) returns value with case-> type" (expect-success
  (import (scheme case-lambda))
  (import (llambda typed))

  (define fixed-lambda
    (case-lambda:
      (((first : <exact-integer>)) 'first)
      (((first : <exact-integer>) (second : <symbol>)) 'second)))

  (ann fixed-lambda (case-> (-> <exact-integer> *) (-> <exact-integer> <symbol> *)))))

(define-test "(case-lambda:) cannot be annotated with incompatible case-> type" (expect-compile-failure
  (import (scheme case-lambda))
  (import (llambda typed))

  (define fixed-lambda
    (case-lambda:
      (((first : <exact-integer>)) 'first)
      (((first : <exact-integer>) (second : <symbol>)) 'second)))

  (ann fixed-lambda (case-> (-> <exact-integer> *) (-> <exact-integer> <string> *)))))

(define-test "R7RS (case-lambda) with wrong arity fails at runtime" (expect-failure
  (import (llambda r7rs-case-lambda))

  (define fixed-lambda
    (r7rs-case-lambda
      ((first) 'first)
      ((first second) 'second)))
  (fixed-lambda 0 1 2)))

(define-test "recursive (case-lambda:)" (expect 5
  (import (llambda typed))

  (define my-gcd (case-lambda:
                (() 0)
                (([a : <exact-integer>]) a)
                (([a : <exact-integer>] [b : <exact-integer>])
                 (if (zero? b)
                   a
                   (my-gcd b (modulo a b))))))

  (my-gcd 15 25)))
