(define-test "starting an empty actor" (expect-success
  (import (llambda actor))

  (start-actor (lambda ()))))

(define-test "actor exiting the process" (expect-exit-value 5
  (import (llambda actor))
  (import (scheme process-context))

  (start-actor (lambda ()
                 (exit 5)))

  ; Block the parent actor
  (read-line)))
