(define-test "direct application with insufficent args fails" (expect-error arity-error?
	(boolean?)))

(define-test "direct application with extraneous args fails" (expect-error arity-error?
	(boolean? #t #f)))

(define-test "applying non-capturing procedure with no arguments" (expect 0
	(apply +)))

(define-test "applying non-capturing procedure with just proper list" (expect 5
	(apply + '(2 3))))

(define-test "applying with non-capturing procdure standalone args and terminal proper list" (expect 6
	(apply + 1 2 '(3))))

(define-test "applying procedure with terminal improper list fails" (expect-error type-error?
	(apply + '(2 3 . 5))))

(define-test "applying procedure with fixed args with terminal static non-list fails" (expect-compile-error type-error?
	(apply - 2)))

(define-test "applying procedure with fixed args with terminal dynamic non-list fails" (expect-error type-error?
	(apply - (typeless-cell 2))))

(define-test "applying procedure without fixed args with terminal static non-list fails" (expect-compile-error type-error?
	(apply + 2)))

(define-test "applying procedure without fixed args with terminal dynamic non-list fails" (expect-error type-error?
	(apply + (typeless-cell 2))))

(define-test "consecutively applying with incompatible arg types fails at compile time" (expect-compile-error type-error?
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

(define-test "applying a typed procedure value with incompatible fixed arg types fails at compile time" (expect-compile-error type-error?
  (import (llambda typed))

  (: apply-binary-op (-> (-> <number> <number> <number>) <number> <symbol> <number>))
  (define (apply-binary-op number-proc op1 op2)
    (number-proc op1 op2))

  (apply-binary-op + 1 'foo)))

(define-test "applying a typed procedure value with incompatible rest arg types fails at compile time" (expect-compile-error type-error?
  (import (llambda typed))

  (: apply-numbers-proc (-> (-> <number> * <number>) <number> <symbol> <number>))
  (define (apply-numbers-proc number-proc op1 op2)
    (number-proc op1 op2))

  (apply-numbers-proc + 1 'foo)))

(define-test "applying a typed procedure value correctly types the return value" (expect-success
  (import (llambda typed))

  (: apply-binary-op (-> (-> <number> <number> <number>) <number> <number> <number>))
  (define (apply-binary-op number-proc op1 op2)
    (ann (number-proc op1 op2) <number>))

  (apply-binary-op + 1 2)))

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
