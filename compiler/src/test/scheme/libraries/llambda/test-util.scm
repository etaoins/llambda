(define-library (llambda test-util)
	(import (scheme base))	
	(export undecided-false undecided-true typeless-cell)
	(begin
	  ; Our optimizer is fairly stupid. This will do for now.
	  (define undecided-false (car (cons #f #t)))
	  (define undecided-true (cdr (cons #f #t)))

	  ; We're actually clever enough to track type information and use that
	  ; for optimization, early errors, etc.
	  ; This is enough to foil the compiler for now
     (define-syntax typeless-cell
       (syntax-rules ()
		  ((typeless-datum x) (car (cons x #f)))))
  )
)
