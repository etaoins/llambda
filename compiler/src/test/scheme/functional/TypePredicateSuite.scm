(define-test "(make-predicate) for intrinsic types" (expect-success
  (import (llambda typed))

  (assert-true ((make-predicate <integer>) 10))
  (assert-true ((make-predicate <number>) 10))
  (assert-true ((make-predicate <any>) 10))

  (assert-false ((make-predicate <integer>) 'test))
  (assert-false ((make-predicate <number>) 'test))
  (assert-true ((make-predicate <any>) 'test))))

(define-test "(make-predicate) for record types" (expect-success
  (import (llambda typed))

  ; Create the type with an initial predicate
	(define-record-type <single-value> (single-value field) single-value?
		(field single-value-field))
	
  (define instance (single-value 1))

  ; Use (make-predicate) here to synthesize a new predicate
  ; This should be identical to the one (define-record-type) created
  (assert-true ((make-predicate <single-value>) instance)) 
  (assert-false ((make-predicate <single-value>) 4))))
  
(define-test "(define-predicate)" (expect-success
  (import (llambda typed))

  (define-predicate my-string? <string>)

  (assert-true (my-string? "Hello, world!"))
  (assert-false (my-string? 'symbol))))



