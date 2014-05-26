(define-test "uncaught exceptions terminate the program" (expect-failure
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
		 (exit 0)))
))

(define-test "exception handlers are called when an exception is raised" (expect-output (handled raised-data)
	(import (scheme write))
	(import (scheme process-context))
	(with-exception-handler 
	  (lambda (obj)
		 (write 'handled)
		 (newline)
		 (write obj)
		 (exit #t))

	  (lambda ()
		 (raise 'raised-data)))
))

(define-test "with-exception-handler returns inner value when no exception is raised" (expect inner-value
	(with-exception-handler
	  (lambda (x))
	  (lambda ()
		 'inner-value))
))

(define-test "exception handlers are called with the dynamic environment of the (raise)" (expect inner-value
	(import (scheme process-context))
	(import (scheme write))

	(define test-param (make-parameter 'outer-value))
	
	(with-exception-handler
	  (lambda (obj)
		 (write (test-param))
		 (exit #t))

	  (lambda ()
		 (parameterize ((test-param 'inner-value))
							(raise 'test))))
))

(define-test "empty lists are not error objects" (expect #f
	(error-object? '())
))

(define-test "(error) raises a new error object" (expect-output (#t "Test error message" (a b (c d e)))
	(import (scheme process-context))
	(import (scheme write))

	(with-exception-handler
	  (lambda (obj)
		 (write (error-object? obj))
		 (newline)
		 (write (error-object-message obj))
		 (newline)
		 (write (error-object-irritants obj))
		 (exit #t))
	  (lambda ()
		 (error "Test error message" 'a 'b (list 'c 'd 'e))))
))

(define-test "if an exception handler returns an exception is raised again" (expect-output (inner outer)
	(import (scheme process-context))
	(import (scheme write))

	(with-exception-handler
	  (lambda (obj)
		 ; "obj" is actually undefined here by R7RS
		 ; llambda reuses th obj from the original exception but that's
		 ; technically an implementation detail
		 (write 'outer)
		 (exit #t))

	  (lambda ()
		 (with-exception-handler 
			(lambda (obj)
			  (write 'inner)
			  (newline))

			(lambda ()
			  (raise 'test)))))

))

(define-test "runtime errors leave the garbage collector in a consistent state" (expect (outer . (one . two))
  (cons 'outer 
    (call/cc (lambda (normal-exit)
      (with-exception-handler
        (lambda (obj)
          (normal-exit (cons 'one 'two)))
        (lambda ()
          ; This will blow up at runtime
          (vector-ref #(1 2 3) (car (cons #f #t))))))))
))
