(define-test "(exit) with #t" (expect-exit-value 0
  (import (llambda process-context))
  (exit #t)))

(define-test "(exit) with #f" (expect-exit-value 255
  (import (llambda process-context))
  (exit #f)))

(define-test "(exit) with 0" (expect-exit-value 0
  (import (llambda process-context))
  (exit 0)))

(define-test "(exit) with 1" (expect-exit-value 1
  (import (llambda process-context))
  (exit 1)))

(define-test "(exit) with 42" (expect-exit-value 42
  (import (llambda process-context))
  (exit 42)))

(define-test "(get-environment-variable)" (expect-success
  (import (llambda process-context))

  (assert-equal "1" (get-environment-variable "LLAMBDA_TEST"))
  (assert-equal #f (get-environment-variable "DOES_NOT_EXIST"))))

(define-test "(get-environment-variables)" (expect-success
  (import (llambda process-context))

  (define env-vars (get-environment-variables))

  (assert-equal '("LLAMBDA_TEST" "1") (assoc "LLAMBDA_TEST" env-vars))
  (assert-equal #f (assoc "DOES_NOT_EXIST" env-vars))))

(define-test "(command-line)" (expect-success
  (import (llambda process-context))

  (define cmdline (command-line))

  ; We should have a single argument for our program name
  (assert-equal 1 (length cmdline))
  (assert-true (string? (car cmdline)))))
