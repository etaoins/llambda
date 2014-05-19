(define-library (llambda test-util)
	(import (scheme base) (scheme write) (scheme process-context))	
	(export undecided-false undecided-true typeless-cell
          assert-equal assert-true assert-false)
	(begin
	  ; Our optimizer is fairly stupid. This will do for now.
	  (define undecided-false (car (cons #f #t)))
	  (define undecided-true (cdr (cons #f #t)))

	  ; We're actually clever enough to track type information and use that
	  ; for optimization, early errors, etc.
	  ; This is enough to foil the compiler for now
     (define-syntax typeless-cell
       (syntax-rules ()
		  ((typeless-datum x) (car (cons x #f)))))

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
