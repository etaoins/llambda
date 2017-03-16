(define-library (llambda test-util)
  (import (scheme base) (scheme write) (scheme process-context))
  (import (llambda typed))
  (export dynamic-false dynamic-true typeless-cell typed-dynamic force-evaluation assert-equal assert-true assert-false
          assert-raises assert-within path-for-test-file)
  (begin
    ; This is similar to (cast) except it destroys any optimiser information about the underlying value
    (define-syntax typed-dynamic
      (syntax-rules ()
                    ((typed-dynamic value <type>)
                     (begin
                       (define mutable : <type> value)
                       (set! mutable value)
                       mutable))))

    ; Use a mutable to launder the value so its type information is lost
    ; There's very little motivation to optimise mutable usage so this should be safe for a long time
    (define (typeless-cell x)
      (typed-dynamic x <any>))

    ; Forces the evaluation of the passed expression
    (define (force-evaluation x)
      (typed-dynamic x <any>))

    (define (dynamic-false)
      (typeless-cell #f))

    (define (dynamic-true)
      (typeless-cell #t))

    (define-syntax assert-equal
      (syntax-rules ()
                    ((assert-equal expected-expr actual-expr)
                     (let ((expected expected-expr) (actual actual-expr))
                       ; Dump the error to stderr
                       (if (not (equal? expected actual)) (parameterize ((current-output-port (current-error-port)))
                                                                        (display "Expected value '")
                                                                        (write expected)
                                                                        (display "' did not match actual value of '")
                                                                        (write actual)
                                                                        (display "' when evaluating '")
                                                                        (write 'actual-expr)
                                                                        (display "'\n")
                                                                        (exit -1)))))))

    (define-syntax assert-true
      (syntax-rules ()
                    ((assert-true actual-expr)
                     (assert-equal #t actual-expr))))

    (define-syntax assert-false
      (syntax-rules ()
                    ((assert-false actual-expr)
                     (assert-equal #f actual-expr))))

    (define-syntax assert-within
      (syntax-rules ()
                    ((assert-within value max-diff actual-expr)
                     (assert-true (< (abs (- value actual-expr)) max-diff)))))

    (define-syntax assert-raises
      (syntax-rules ()
                    ((assert-raises pred? body ...)
                     (guard (condition
                              ((pred? condition) #t)
                              (else
                                (parameterize ((current-output-port (current-error-port)))
                                              (display "Expected exception satisfying ")
                                              (display 'pred?)
                                              (display " to be raised but '")
                                              (write condition)
                                              (display "' was raised")
                                              (exit -1))))
                            body ...
                            (parameterize ((current-output-port (current-error-port)))
                                          (display "Expected exception to be raised but execution completed normally")
                                          (exit -1))))))

  (define (path-for-test-file [filename : <string>])
    (string-append (get-environment-variable "LLAMBDA_TEST_FILES_BASE") "/" filename))))
