(define-test "datum can be cast to pair" (expect 1
	(import (scheme core))
	; This assumes (vector-ref) takes a boxed pair
	(vector-ref (car '(#(1) . #f)) 0)))
