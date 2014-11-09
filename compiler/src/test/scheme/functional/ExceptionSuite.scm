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

(define-test "exception handler with incorrect thunk arity fails at compile time" (expect-compile-failure
	(import (scheme process-context))
	(with-exception-handler 
	  (lambda (obj)
		 (exit #f))

	  (lambda (too many args)
		 (raise 'raised-data)))
))

(define-test "exception handler with incorrect handler arity fails at compile time" (expect-compile-failure
	(import (scheme process-context))
	(with-exception-handler 
	  (lambda ()
		 (exit #f))

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

(define-test "exception handlers can have their continuation captured" (expect-output ("About to continute" "Original exception")
  (import (scheme base))
  (import (scheme write))
  (import (scheme process-context))

  (with-exception-handler
    (lambda (except)
      ; 6) Output the original exeption 
      (write except)
      (exit #t))
    (lambda ()
      ; 4) Capture this continuation in to something that can rethrow the original exception
      (define rethrower
        (call/cc 
          (lambda (exit-all)
            (with-exception-handler
              (lambda (except)
                ; 2) Immediately try to capture a continuation
                (call/cc
                  (lambda (cont)
                    ; 3) Pass the continuation to #4
                    (exit-all cont))))

              (lambda ()
                ; 1) Raise an exception to be caught by the handler
                (raise "Original exception"))))))
      ; 5) Rethrow by calling the exception handler's continuation
      (write "About to continute")
      (newline)
      (rethrower)))))

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

(define-test "(error) does not raise a (file-error?)" (expect-success
  (guard (obj
           (else
             (assert-false (file-error? obj))))
         (error "Test error!"))))

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
          (vector-ref #(1 2 3) dynamic-true))))))
))

(define-test "(raise-continuable)" (expect-output ("should be a number" 65)
	(import (scheme write))
  (write
    (with-exception-handler
      (lambda (con)
        (cond
          ((string? con)
           (write con)
           (newline))
          (else
            (write "a warning has been issued")
            (newline)))
        42)
      (lambda ()
        (+ (raise-continuable "should be a number")
           23))))))

(define-test "(guard)" (expect-success
  ; This matches the first clause
  (assert-equal 42 
    (guard (condition
             ((assq 'a condition) => cdr)
             ((assq 'b condition)))
           (raise (list (cons 'a 42)))))

  ; This matches the second clause
  (assert-equal '(b . 23)
    (guard (condition
             ((assq 'a condition) => cdr)
             ((assq 'b condition)))
           (raise (list (cons 'b 23)))))
  
  ; This matches the else
  (assert-equal 'fallthrough
    (guard (condition
             ((assq 'a condition) => cdr)
             ((assq 'b condition))
             (else 'fallthrough))
           (raise (list (cons 'c 23)))))
  
  ; This doesn't invoke the handler it all and returns a single value
  (assert-equal 'no-except
    (guard (condition
             ((assq 'a condition) => cdr)
             ((assq 'b condition))
             (else 'fallthrough))
           'no-except))

  ; This doesn't invoke the handler it all and returns a multiple values
  (assert-equal '(1 2 3)
    (call-with-values
      (lambda ()
        (guard (condition
                 ((assq 'a condition) => cdr)
                 ((assq 'b condition))
                 (else 'fallthrough))
               (values 1 2 3)))
      (lambda args args)))

  ; This doesn't match any clause and re-throws to the outer exception handler
  (assert-equal '(outer-caught original-datum)
    (call/cc (lambda (return)
      (with-exception-handler
        (lambda (obj)
          (return (list 'outer-caught obj)))
        (lambda ()
          (guard (condition
                   ((string? condition) 'matched))
                 (raise 'original-datum)))))))

  ; Make sure the guard conditions are evaluated in the dynamic environment of the guard
  (define test-parameter (make-parameter 'default))
  (parameterize ((test-parameter 'outer1))
    (assert-equal 'outer1
      (guard (condition
               ((assq 'a condition) => cdr)
               ((assq 'b condition))
               (else (test-parameter)))
             (parameterize ((test-parameter 'inner1))
               (raise (list (cons 'c 23)))))))))

(define-test "R7RS definition of (guard) works as defined" (expect-success
  (define-syntax r7rs-guard
    (syntax-rules ()
                  ((r7rs-guard (var clause ...) e1 e2 ...)
                   ((call/cc
                      (lambda (r7rs-guard-k)
                        (with-exception-handler
                          (lambda (condition)
                            ((call/cc
                               (lambda (handler-k)
                                 (r7rs-guard-k
                                   (lambda ()
                                     (let ((var condition))
                                       (r7rs-guard-aux
                                         (handler-k
                                           (lambda ()
                                             (raise condition)))
                                         clause ...))))))))
                          (lambda ()
                            (call-with-values
                              (lambda () e1 e2 ...)
                              (lambda args
                                (r7rs-guard-k
                                  (lambda ()
                                    (apply values args)))))))))))))
  (define-syntax r7rs-guard-aux
    (syntax-rules (else =>)
                  ((r7rs-guard-aux reraise (else result1 result2 ...))
                   (begin result1 result2 ...))
                  ((r7rs-guard-aux reraise (test => result))
                   (let ((temp test))
                     (if temp
                       (result temp)
                       reraise)))
                  ((r7rs-guard-aux reraise (test => result)
                              clause1 clause2 ...)
                   (let ((temp test))
                     (if temp
                       (result temp)
                       (r7rs-guard-aux reraise clause1 clause2 ...))))
                  ((r7rs-guard-aux reraise (test))
                   (or test reraise))
                  ((r7rs-guard-aux reraise (test) clause1 clause2 ...)
                   (let ((temp test))
                     (if temp
                       temp
                       (r7rs-guard-aux reraise clause1 clause2 ...))))
                  ((r7rs-guard-aux reraise (test result1 result2 ...))
                   (if test
                     (begin result1 result2 ...)
                     reraise))
                  ((r7rs-guard-aux reraise
                              (test result1 result2 ...)
                              clause1 clause2 ...)
                   (if test
                     (begin result1 result2 ...)
                     (r7rs-guard-aux reraise clause1 clause2 ...)))))

  ; This matches the first clause
  (assert-equal 42 
    (r7rs-guard (condition
             ((assq 'a condition) => cdr)
             ((assq 'b condition)))
           (raise (list (cons 'a 42)))))

  ; This matches the second clause
  (assert-equal '(b . 23)
    (r7rs-guard (condition
             ((assq 'a condition) => cdr)
             ((assq 'b condition)))
           (raise (list (cons 'b 23)))))
  
  ; This matches the else
  (assert-equal 'fallthrough
    (r7rs-guard (condition
             ((assq 'a condition) => cdr)
             ((assq 'b condition))
             (else 'fallthrough))
           (raise (list (cons 'c 23)))))
  
  ; This doesn't invoke the handler it all and returns a single value
  (assert-equal 'no-except
    (r7rs-guard (condition
             ((assq 'a condition) => cdr)
             ((assq 'b condition))
             (else 'fallthrough))
           'no-except))

  ; This doesn't invoke the handler it all and returns a multiple values
  (assert-equal '(1 2 3)
    (call-with-values
      (lambda ()
        (r7rs-guard (condition
                 ((assq 'a condition) => cdr)
                 ((assq 'b condition))
                 (else 'fallthrough))
               (values 1 2 3)))
      (lambda args args)))

  ; This doesn't match any clause and re-throws to the outer exception handler
  (assert-equal '(outer-caught original-datum)
    (call/cc (lambda (return)
      (with-exception-handler
        (lambda (obj)
          (return (list 'outer-caught obj)))
        (lambda ()
          (r7rs-guard (condition
                   ((string? condition) 'matched))
                 (raise 'original-datum)))))))

  ; Make sure the guard conditions are evaluated in the dynamic environment of the guard
  (define test-parameter (make-parameter 'default))
  (parameterize ((test-parameter 'outer1))
    (assert-equal 'outer1
      (r7rs-guard (condition
               ((assq 'a condition) => cdr)
               ((assq 'b condition))
               (else (test-parameter)))
             (parameterize ((test-parameter 'inner1))
               (raise (list (cons 'c 23)))))))))
