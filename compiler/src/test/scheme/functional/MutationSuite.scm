(define-test "trivial mutation" (expect 2
	(import (scheme core))
	(define x 1)
	(set! x 2)
	x))

(define-test "mutatation across branches" (expect 3
	(import (scheme core))
	(define x 1)
	(if #f
	  (set! x 2)
	  (set! x 3))
	x))

