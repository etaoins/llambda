(define-test "+ is a procedure" (expect #t
	(procedure? +)))

(define-test "#f is not a procedure" (expect #f
	(procedure? #f)))

(define-test "non-constant procedure values can be applied" (expect 10
	(import (llambda test-util))

	(define math-op
		; This is + but our optimizer won't know that
		; That means this has to be converted to a function value at runtime

		; Also note that + and / have different signatures which will have to be
		; normalized when they're boxed. This doesn't matter for our current
		; implementation but it may matter if we try to be more tricky
		(if undecided-true + /))

	(math-op 2 3 5)))

(define-test "datum cells can be applied" (expect 10
	(import (llambda test-util))

	((typeless-cell -) 80 50 20)))

(define-test "applying datum cells with too many arguments fails" (expect-failure
	(import (llambda test-util))

	((typeless-cell exact?) 80 20)))

(define-test "applying datum cells with insufficient arguments fails" (expect-failure
	(import (llambda test-util))

	((typeless-cell exact?))))

(define-test "procedure returning nothing" (expect #!unit
	(define (return-nothing))
	(return-nothing)))

(define-test "procedure returning single value" (expect 7
	(define (return-7) 7)
	(return-7)))

(define-test "procedure returning its only argument" (expect 7
	(define (return-value value) value)
	(return-value 7)))

(define-test "procedure adding its arguments" (expect 7
	(define (add-two-values a b) (+ a b))
	(add-two-values 4 3)))

(define-test "procedure mutating args does not affect caller" (expect 7
	(define (set-arg a) (set! a -89))
	
	(define outer-value 7)
	(set-arg outer-value)
	outer-value))

(define-test "procedure mutating rest arg itself" (expect 3
    (define (set-rest-arg . rest)
      (set! rest '(1 2 3))
      (length rest))

    (set-rest-arg 1 2 3 4 5)))

; R7RS requires the procedure to be passed a newly allocated list
; This is very annoying for optimisation reasons so at least make sure it works
(define-test "procedure mutating rest list contents" (expect (3 2)
    (define (mutate-rest-arg . rest)
      (set-car! rest 3)
      rest)

    (mutate-rest-arg 1 2)))

(define-test "capturing constants" (expect 7
	(define two 2)
	
	(define (add-two value)
	  (+ two value))

	(add-two 5)))

(define-test "capturing immutable variables" (expect 7
	(define (create-adder-proc to-add)
	  (lambda (value)
		 (+ to-add value)))

	(define add-five (create-adder-proc 5))
	(add-five 2)))

(define-test "capturing mutable variables" (expect (1 2 1 3 2 3)
	(define (create-counter)
	  (define value 0)
	  (lambda ()
		 (set! value (+ value 1))
		 value))

	(define first-counter (create-counter))
	(define second-counter (create-counter))

	(define first-value (first-counter))  ; 1
	(define second-value (first-counter)) ; 2
	(define third-value (second-counter)) ; 1
	(define fourth-value (first-counter)) ; 3
	(define fifth-value (second-counter)) ; 2
	(define sixth-value (second-counter)) ; 3
	

	(list first-value second-value third-value fourth-value fifth-value sixth-value)))

(define-test "setting mutable variable to procedure argument" (expect 10
	(define outer-value 0)
	(apply 
	  (lambda (value-to-set)
		 (set! outer-value value-to-set))
	  '(10))

	outer-value))

(define-test "capturing another lambda" (expect (2 4 6)
	(define (create-multiplier factor)
	  (lambda (value)
		 (* factor value)))

	(define (create-transformed-counter transformer)
	  (define value 0)
	  (lambda ()
		 (set! value (+ value 1))
		 (transformer value)))

	; Create a transformer
	(define times-two (create-multiplier 2))
	; Pass the transformer to our counter
	(define counter (create-transformed-counter times-two))

	(define first-value (counter))
	(define second-value (counter))
	(define third-value (counter))
	
	(list first-value second-value third-value)))
