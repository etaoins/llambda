(define-library (scheme process-context)
	(import (llambda nfi))
    (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))

    (export exit emergency-exit)
    (begin
      (define-r7rs exit (world-function "lliby_exit" (<datum-cell>)))
      (define-r7rs emergency-exit (native-function "lliby_emergency_exit" (<datum-cell>))))
)
