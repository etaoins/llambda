(define-test "uncaught exceptions terminate the program" (expect-exit-value 255
  (raise 0)))

(define-test "uncaught exceptions unwind all states" (expect-output (two one)
  (import (scheme write))
  (import (scheme process-context))
  (dynamic-wind
    (lambda ())
    (lambda ()
     (dynamic-wind
      (lambda ())
      (lambda ()
        (raise 0)
        (dynamic-wind
         (lambda ())
         (lambda ())
         (lambda ()
          (write 'three)
          (newline))))
      (lambda ()
        (write 'two)
        (newline))))
    (lambda ()
     (write 'one)
     (newline)
     (exit 0)))))

(define-test "empty lists are not error objects" (expect #f
  (error-object? '())))

(define-test "empty lists are not out of memory errors" (expect #f
  (import (llambda error))
  (out-of-memory-error? '())))

(define-test "(error) does not raise a (file-error?)" (expect-success
  (guard (obj
           (else
             (assert-false (file-error? obj))))
         (error "Test error!"))))

(define-test "(raise-read-error)" (expect-success
  (import (llambda error))

  (assert-raises read-error?
                 (raise-read-error "Oops!"))))

(define-test "(guard)" (expect-success
  ; This matches the first clause
  (assert-equal 42
    (guard (condition
             ((assv 'a condition) => cdr)
             ((assv 'b condition)))
           (raise (list (cons 'a 42)))))

  ; This matches the second clause
  (assert-equal '(b . 23)
    (guard (condition
             ((assv 'a condition) => cdr)
             ((assv 'b condition)))
           (raise (list (cons 'b 23)))))

  ; This matches the else
  (assert-equal 'fallthrough
    (guard (condition
             ((assv 'a condition) => cdr)
             ((assv 'b condition))
             (else 'fallthrough))
           (raise (list (cons 'c 23)))))

  ; This doesn't invoke the handler it all and returns a single value
  (assert-equal 'no-except
    (guard (condition
             ((assv 'a condition) => cdr)
             ((assv 'b condition))
             (else 'fallthrough))
           'no-except))

  ; Make sure the guard conditions are evaluated in the dynamic environment of the guard
  (define test-parameter (make-parameter 'default))
  (parameterize ((test-parameter 'outer1))
    (assert-equal 'outer1
      (guard (condition
               ((assv 'a condition) => cdr)
               ((assv 'b condition))
               (else (test-parameter)))
             (parameterize ((test-parameter 'inner1))
               (raise (list (cons 'c 23)))))))))
