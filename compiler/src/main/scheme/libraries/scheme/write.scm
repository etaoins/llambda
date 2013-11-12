(define-library (scheme write)
	(import (llambda nfi))
	(import (scheme base))
	(import (rename (llambda internal) (define-report-procedure define-r7rs)))
	
	(export write)
	(begin
	  (define-r7rs write (native-function "lliby_write" (boxed-datum) void)))
)
