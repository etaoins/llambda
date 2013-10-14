(define-test "non-empty vector is a vector" (expect #t
	(import (scheme core))
	(vector? #(1 2 3))))

(define-test "empty vector is a vector" (expect #t
	(import (scheme core))
	(vector? #())))

(define-test "exact integer is not a vector" (expect #f
	(import (scheme core))
	(vector? 4)))

(define-test "construct empty vector" (expect #()
	(import (scheme core))
	(make-vector 0 #f)))

(define-test "construct filled vector" (expect #(5.0 5.0 5.0)
	(import (scheme core))
	(make-vector 3 5.0)))

(define-test "vector-ref" (expect c
	(import (scheme core))
	(vector-ref #(a b c d e f) 2)))

(define-test "vector-ref out of bounds fails" (expect-failure
	(import (scheme core))
	(vector-ref #(a b c d e f) 7)))

(define-test "vector-ref with non-integer fails" (expect-failure
	(import (scheme core))
	(vector-ref #(a b c d e f) "4")))

(define-test "vector-set!" (expect #(1 1 2 1 1)
	(import (scheme core))
	; Need to make a new vector because vector literals are immutable
	(define test-vector (make-vector 5 1))
	(vector-set! test-vector 2 2)
	test-vector))
