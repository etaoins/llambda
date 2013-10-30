(define-library (scheme inexact)
	(import (llambda nfi))
	(import (scheme core))
	(import (rename (llambda internal) (define-report-procedure define-r7rs)))
	
	(export write)
	(begin
	  (define-r7rs write (native-function "lliby_write" (boxed-datum) void)))
)
