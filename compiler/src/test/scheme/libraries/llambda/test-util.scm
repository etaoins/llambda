(define-library (llambda test-util)
	(import (scheme base))	
	(export undecided-false undecided-true)
	(begin
	  ; Our optimizer is ridiculously stupid. This will do for now.
	  (define undecided-false #f)
	  (define undecided-true #t))
)