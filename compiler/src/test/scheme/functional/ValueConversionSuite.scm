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
