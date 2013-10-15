(define-test "datum can be cast to pair" (expect 1
	(import (scheme core))
	; This assumes (vector-ref) takes a boxed pair
	(vector-ref (car '(#(1) . #f)) 0)))

(define-test "#false can be unboxed as boolean" (expect #t
	(import (scheme core))
	; This assumes (not) takes an unboxed boolean
	(not (car '(#f . #f)))))

(define-test "#true can be unboxed as boolean" (expect #f
	(import (scheme core))
	; This assumes (not) takes an unboxed boolean
	(not (car '(#t . #f)))))

(define-test "empty list can be unboxed as boolean" (expect #f
	(import (scheme core))
	; This assumes (not) takes an unboxed boolean
	(not (car '('() . #f)))))

(define-test "exact int can be unboxed as integer" (expect #(#t #t #t)
	(import (scheme core))
	; This assumes (make-vector) takes an unboxed exact integer
	(make-vector (car '(3 . #f)) #t)))

(define-test "inexact rational cannot be unboxed as integer" (expect-failure
	(import (scheme core))
	; This assumes (make-vector) takes an unboxed exact integer
	(make-vector (car '(3.0 . #f)) #t)))
