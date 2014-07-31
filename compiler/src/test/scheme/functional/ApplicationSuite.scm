(define-test "direct application with insufficent args fails" (expect-failure
	(boolean?)))

(define-test "direct application with extraneous args fails" (expect-failure
	(boolean? #t #f)))

(define-test "applying non-capturing procedure with no arguments" (expect 0
	(apply +)))

(define-test "applying non-capturing procedure with just proper list" (expect 5
	(apply + '(2 3))))

(define-test "applying with non-capturing procdure standalone args and terminal proper list" (expect 6
	(apply + 1 2 '(3))))

(define-test "applying procedure with terminal improper list fails" (expect-failure
	(apply + '(2 3 . 5))))

(define-test "applying procedure with terminal non-list fails" (expect-failure
	(apply - 2)))

(define-test "consecutively applying with incompatible arg types fails at compile time" (expect-compile-failure
  (import (llambda typed))
  (define typeless-vector (typeless-cell #(1 2 3)))
  (define typeless-1 (typeless-cell 1))

  ; typeless-vector should be typed as a vector after this
  (vector-ref typeless-vector typeless-1)
  ; The type system should notice this is illegal
  (* typeless-vector typeless-1)))

; This is testing a very specific bug in PlanApplication
(define-test "applying a procedure with an unknown list only evaluates the arg expression once" (expect-success
  (define counter 0)

  (define (inc-and-return-list)
    (set! counter (+ counter 1))
    '(2 3))

  (define result
    (apply * (inc-and-return-list)))

  (assert-equal 6 result)
  (assert-equal 1 counter)))

(define-test "nested apply" (expect 3
	(apply apply (list + '(1 2)))))

(define-test "applying a capturing procedure" (expect 7
	(define (create-adder-proc to-add)
	  (lambda (value)
		 (+ to-add value)))

	(apply (create-adder-proc 5) '(2))))

; This is from R7RS except sqrt is replaced by - due to lack of sqrt
(define-test "composing using apply" (expect -900
	(define compose
	  (lambda (f g)
		 (lambda args
			(f (apply g args)))))
	((compose - *) 12 75)))
