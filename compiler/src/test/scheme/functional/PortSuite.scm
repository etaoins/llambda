(define-test "(current-input-port)" (expect-success
  (assert-true  (port? (current-input-port)))
  (assert-true  (input-port? (current-input-port)))
  (assert-false (output-port? (current-input-port)))
  (assert-true  (input-port-open? (current-input-port)))
  (assert-false (output-port-open? (current-input-port)))))

(define-test "(current-output-port)" (expect-success
  (assert-true  (port? (current-output-port)))
  (assert-false (input-port? (current-output-port)))
  (assert-true  (output-port? (current-output-port)))
  (assert-false (input-port-open? (current-output-port)))
  (assert-true  (output-port-open? (current-output-port)))))

(define-test "(current-error-port)" (expect-success
  (assert-true  (port? (current-error-port)))
  (assert-false (input-port? (current-error-port)))
  (assert-true  (output-port? (current-error-port)))
  (assert-false (input-port-open? (current-error-port)))
  (assert-true  (output-port-open? (current-error-port)))))

(define-test "(input-port-open?) on non-port fails" (expect-compile-error type-error?
  (input-port-open? #f)))

(define-test "(output-port-open?) on non-port fails" (expect-compile-error type-error?
  (output-port-open? #f)))

(define-test "(close-input-port)" (expect-success
  (define input-port (current-input-port))

  (assert-true (input-port-open? input-port))
  (assert-true (input-port? input-port))

  (close-input-port input-port)

  (assert-false (input-port-open? input-port))
  (assert-true (input-port? input-port))

  ; This should succeed
  (close-input-port input-port)))

(define-test "(close-input-port) on output port fails" (expect-error invalid-argument-error?
  (close-input-port (open-output-string))))

(define-test "(close-output-port)" (expect-success
  (define output-port (open-output-string))

  (assert-true (output-port-open? output-port))
  (assert-true (output-port? output-port))

  (close-output-port output-port)

  (assert-false (output-port-open? output-port))
  (assert-true (output-port? output-port))

  ; This should succeed
  (close-output-port output-port)))

(define-test "(close-output-port) on input port fails" (expect-error invalid-argument-error?
  (close-output-port (current-input-port))))

(define-test "(close-port)" (expect-success
  (define input-port (current-input-port))
  (assert-true (input-port-open? input-port))
  (assert-true (input-port? input-port))
  (close-port input-port)
  (assert-false (input-port-open? input-port))

  (define output-port (open-output-string))
  (assert-true (output-port-open? output-port))
  (assert-true (output-port? output-port))
  (close-port output-port)
  (assert-false (output-port-open? output-port))))

(define-test "writing to an open input port fails" (expect-error invalid-argument-error?
  (import (scheme write))
  (write "Test" (current-input-port))))

(define-test "writing to a closed ouput port fails" (expect-error invalid-argument-error?
  (import (scheme write))

  (define output-port (open-output-string))
  (close-output-port output-port)
  (write "Test" output-port)))

(define-test "ports can be parameterized" (expect #t
  (parameterize ((current-output-port (current-error-port)))
    (eqv? (current-output-port) (current-error-port)))))

(define-test "output string ports" (expect "piece by piece by piece.\n"
  (import (scheme write))

  (parameterize
    ((current-output-port
       (open-output-string)))
    (display "piece")
    (display " by piece ")
    (display "by piece.")
    (newline)
    (get-output-string (current-output-port)))))

(define-test "(get-output-string) fails on non-output string port" (expect-error invalid-argument-error?
  (get-output-string (open-output-bytevector))))

(define-test "output bytevector ports" (expect #u8(#x70 #x69 #x65 #x63 #x65 #x20 #x62 #x79 #x20 #x70 #x69 #x65 #x63 #x65 #x20 #x62 #x79 #x20 #x70 #x69 #x65 #x63 #x65 #x2e #x0a)
  (import (scheme write))

  (parameterize
    ((current-output-port
       (open-output-bytevector)))
    (display "piece")
    (display " by piece ")
    (display "by piece.")
    (newline)
    (get-output-bytevector (current-output-port)))))

(define-test "(get-output-bytevector) fails on non-output bytevector port" (expect-error invalid-argument-error?
  (get-output-bytevector (open-output-string))))

(define-test "(open-input-string)" (expect-success
  (define input-string (open-input-string "Hello!"))

  (assert-true (port? input-string))
  (assert-true (input-port? input-string))
  (assert-true (input-port-open? input-string))

  (close-port input-string)

  (assert-true  (port? input-string))
  (assert-true  (input-port? input-string))
  (assert-false (input-port-open? input-string))))

(define-test "(open-input-bytevector)" (expect-success
  (define input-bytevector (open-input-bytevector #u8(#x48 #x65 #x6c #x6c #x6f #x21)))

  (assert-true (port? input-bytevector))
  (assert-true (input-port? input-bytevector))
  (assert-true (input-port-open? input-bytevector))

  (close-port input-bytevector)

  (assert-true  (port? input-bytevector))
  (assert-true  (input-port? input-bytevector))
  (assert-false (input-port-open? input-bytevector))))

(define-test "simple (call-with-port)" (expect-success
  (define simple-port (open-input-string ""))

  ; If the procedure returns we should close the port
  (define result-value (call-with-port simple-port
                                       (lambda (port)
                                         (assert-true (input-port-open? port))
                                         'success)))

  (assert-equal result-value 'success)
  (assert-false (input-port-open? simple-port))))
