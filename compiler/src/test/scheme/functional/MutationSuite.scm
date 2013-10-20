(define-test "trivial mutation" (expect 2
	(import (scheme core))
	(define x 1)
	(set! x 2)
	x))

(define-test "mutatation across branches" (expect 3
	(import (scheme core))
	(import (llambda test-util))
	(define x 1)
	(if undecided-false
	  (set! x 2)
	  (set! x 3))
	x))

