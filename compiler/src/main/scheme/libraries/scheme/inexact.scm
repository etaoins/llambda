(define-library (scheme inexact)
	(import (llambda nfi))
	(import (scheme base))
	(import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))

	(export cos sin tan)
	(begin
	  ; These always return inexact numbers so we can use the C standard
	  ; library directly
	  (define-r7rs sin (native-function "sin" (<double>) <double>))
	  (define-r7rs cos (native-function "cos" (<double>) <double>))
	  (define-r7rs tan (native-function "tan" (<double>) <double>)))
)	  
