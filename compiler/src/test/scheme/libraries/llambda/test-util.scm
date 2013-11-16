(define-library (llambda test-util)
	(import (scheme base))	
	(export undecided-false undecided-true typeless-boxed)
	(begin
	  ; Our optimizer is ridiculously stupid. This will do for now.
	  (define undecided-false #f)
	  (define undecided-true #t)

	  ; We're actually clever enough to track type information and use that
	  ; for optimization, early errors, etc.
	  ; This is enough to foil the compiler for now
     (define-syntax typeless-boxed
       (syntax-rules ()
		  ((typeless-datum x) (car (cons x #f)))))
  )
)
