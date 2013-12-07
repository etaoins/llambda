(define-library (test singleexpr)
	(import (llambda internal primitives))
	(export a)
	(begin 
	  (define a 1)))
