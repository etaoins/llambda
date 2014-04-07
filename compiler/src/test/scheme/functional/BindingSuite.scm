(define-test "simple let*" (expect 70
	; This is taken from R7RS
	(let ((x 2) (y 3))
	  (let* ((x 7)
			 (z (+ x y)))
		(* z x)))))
