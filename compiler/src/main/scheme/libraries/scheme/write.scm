(define-library (scheme write)
	(import (llambda nfi))
    (import (scheme base))
    (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))

    (export write)
    (begin
      (define-r7rs write (native-function "lliby_write" (<datum-cell>))))
)
