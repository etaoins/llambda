(define-test "(current-input-port)" (expect-success
  (assert-true  (port? (current-input-port)))
  (assert-true  (input-port? (current-input-port)))
  (assert-false (output-port? (current-input-port)))
  (assert-true  (textual-port? (current-input-port)))
  (assert-true  (binary-port? (current-input-port)))))

(define-test "(current-output-port)" (expect-success
  (assert-true  (port? (current-output-port)))
  (assert-false (input-port? (current-output-port)))
  (assert-true  (output-port? (current-output-port)))
  (assert-true  (textual-port? (current-output-port)))
  (assert-true  (binary-port? (current-output-port)))))

(define-test "(current-error-port)" (expect-success
  (assert-true  (port? (current-error-port)))
  (assert-false (input-port? (current-error-port)))
  (assert-true  (output-port? (current-error-port)))
  (assert-true  (textual-port? (current-error-port)))
  (assert-true  (binary-port? (current-error-port)))))

(define-test "writing to an input port fails" (expect-failure
  (import (scheme write))
  (write "Test" (current-input-stream))))

(define-test "ports can be parameterized" (expect #t
	(parameterize ((current-output-port (current-error-port)))
    (eqv? (current-output-port) (current-error-port)))))
