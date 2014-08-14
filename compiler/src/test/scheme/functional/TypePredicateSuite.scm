(define-test "(make-predicate) for intrinsic types" (expect-success
  (import (llambda typed))

  (assert-true ((make-predicate <exact-integer>) 10))
  (assert-true ((make-predicate <number>) 10))
  (assert-true ((make-predicate <any>) 10))

  (assert-false ((make-predicate <exact-integer>) 'test))
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

(define-test "(define-predicate) for boolean constants" (expect-success
  (import (llambda typed))

  (define-predicate false? #f)
  (define-predicate true? #t)

  (assert-false (true? "Hello, world!"))
  (assert-false (true? #f))
  (assert-true  (true? #t))
  
  (assert-false (false? "Hello, world!"))
  (assert-true  (false? #f))
  (assert-false (false? #t))))

(cond-expand (immutable-pairs
  (define-test "(define-predicate) for pairs" (expect-success
    (import (llambda typed))

    (define-predicate any-pair? (Pair <any> <any>))
    (define-predicate string-symbol-pair? (Pair <string> <symbol>))
    (define-predicate symbol-string-pair? (Pair <symbol> <string>))

    (assert-true  (any-pair? '(1 . 2)))
    (assert-true  (any-pair? '(foo . "bar")))
    (assert-true  (any-pair? '("bar" . foo)))
    
    (assert-false (string-symbol-pair? '(1 . 2)))
    (assert-false (string-symbol-pair? '(foo . "bar")))
    (assert-true  (string-symbol-pair? '("bar" . foo)))
    
    (assert-false (symbol-string-pair? '(1 . 2)))
    (assert-true  (symbol-string-pair? '(foo . "bar")))
    (assert-false (symbol-string-pair? '("bar" . foo)))
    
    (define-predicate two-number-proper-list? (Pair <number> (Pair <number> <empty-list>)))

    (assert-true (two-number-proper-list? '(1 2)))
    (assert-true (two-number-proper-list? '(1.0 -5)))
    (assert-false (two-number-proper-list? '(1 . 2)))
    (assert-false (two-number-proper-list? '(1 sneaky-symbol)))))))

(cond-expand (immutable-pairs
  (define-test "(define-predicate) for lists" (expect-success
    
  (import (llambda typed))
    
  (define-predicate string-list? (Listof <string>))
  (define-predicate symbol-list? (Listof <symbol>))

  (assert-true  (string-list? '("one" "two")))
  (assert-false (string-list? '(one two)))
  (assert-false (string-list? '(1 2)))

  (assert-false (symbol-list? '("one" "two")))
  (assert-true  (symbol-list? '(one two)))
  (assert-false (symbol-list? '(1 2)))))))

