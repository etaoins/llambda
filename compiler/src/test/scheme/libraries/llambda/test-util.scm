(define-library (llambda test-util)
  (import (scheme base) (scheme write) (scheme process-context))  
  (export dynamic-false dynamic-true typeless-cell
          assert-equal assert-true assert-false)
  (begin
    ; Use a mutable to launder the value so its type information is lost
    ; There's very little motivation to optimise mutable usage so this should be safe for a long time
    (define (typeless-cell x)
      (define mutable #!unit)
      (set! mutable x)
      mutable)

    (define dynamic-false (typeless-cell #f))
    (define dynamic-true (typeless-cell #t))

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
                                                                        (display "'")
                                                                        (display " when evaluating '")
                                                                        (write 'actual-expr)
                                                                        (display "'\n")
                                                                        (exit -1)))))))

    (define-syntax assert-true
      (syntax-rules ()
                    ((assert-true actual-expr)
                     (assert-equal #t actual-expr))))

    (define-syntax assert-false
      (syntax-rules ()
                    ((assert-true actual-expr)
                     (assert-equal #f actual-expr))))
  )
)
