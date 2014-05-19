(define-test "current input port is a port" (expect #t
  (port? (current-input-port))))

(define-test "current output port is a port" (expect #t
  (port? (current-output-port))))

(define-test "current error port is a port" (expect #t
  (port? (current-error-port))))

(define-test "current input port is an input port" (expect #t
  (input-port? (current-input-port))))

(define-test "current output port is not an input port" (expect #f
  (input-port? (current-output-port))))

(define-test "current error port is not an input port" (expect #f
  (input-port? (current-error-port))))

(define-test "current input port is not a ouput port" (expect #f
  (output-port? (current-input-port))))

(define-test "current output port is a output port" (expect #t
  (output-port? (current-output-port))))

(define-test "current error port is a output port" (expect #t
  (output-port? (current-error-port))))

(define-test "writing to an input port fails" (expect-failure
  (import (scheme write))
  (write "Test" (current-input-stream))))

(define-test "ports can be parameterized" (expect #t
	(parameterize ((current-output-port (current-error-port)))
    (eqv? (current-output-port) (current-error-port)))))
