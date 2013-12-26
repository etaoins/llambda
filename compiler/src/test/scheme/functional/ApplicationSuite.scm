(define-test "application with insufficent args fails" (expect-failure
	(boolean?)))

(define-test "application with extraneous args fails" (expect-failure
	(boolean? #t #f)))

(define-test "applying non-capturing procedure with proper list" (expect 5
	(apply + '(2 3))))

; The wording in R7RS 6.10 makes me think this is wrong but I can't decode
; what it's trying to say. mit-scheme doesn't allow improper lists at all.
(define-test "applying procedure with improper list fails" (expect-failure
	(apply + '(2 3 . 5))))

(define-test "applying procedure with non-list fails" (expect-failure
	(apply - 2)))

(define-test "applying with too many arguments fails" (expect-failure
	(apply exact? '(1 2))))

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
