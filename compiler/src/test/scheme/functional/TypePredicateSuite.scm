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

(define-test "(define-predicate) for custom unions" (expect-success
  (import (llambda typed))

  (define-predicate string-or-number? (U <string> <number>))

  (assert-true (string-or-number? "Hello"))
  (assert-true (string-or-number? (typeless-cell "Hello")))
  (assert-true (string-or-number? 5))
  (assert-true (string-or-number? 12.0))
  (assert-false (string-or-number? #f))))

(define-test "(define-predicate) for unions of record types" (expect-success
  (import (llambda typed))
	
  (define-record-type <record1> (record1) record1?)
  (define-record-type <record2> (record2) record2?)
  (define-record-type <record3> (record3) record3?)

  (define-type <custom-union> (U <record1> <record2>))
  (define-predicate custom-union? <custom-union>)

  (assert-true (custom-union? (typeless-cell (record1))))
  (assert-true (custom-union? (typeless-cell (record2))))
  (assert-false (custom-union? (typeless-cell (record3))))
  (assert-false (custom-union? #f))))
