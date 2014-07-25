(define-test "constructing empty record type" (expect #t
	(define-record-type <unit> (unit) unit?)
	(unit? (unit))))

(define-test "record types are disjoint" (expect #f
	(define-record-type <type1> (type1) type1?)
	(define-record-type <type2> (type2) type2?)
	(type1? (type2))))

(define-test "record types with same source name are disjoint" (expect #f
	(define type1? ((lambda ()
		(define-record-type <same-name> (same-name) same-name?)
		same-name?)))

	(define type2-cons ((lambda ()
		(define-record-type <same-name> (same-name) same-name?)
		same-name)))
	
	(type1? (type2-cons))))

(define-test "constructing record type with one typeless initialized immutable field" (expect 1
	(define-record-type <single-value> (single-value field) single-value?
		(field single-value-field))
	
	(define instance (single-value 1))
	(single-value-field instance)))

(define-test "record types can be converted to and from typeless data" (expect 1
	(import (llambda test-util))

	(define-record-type <single-value> (single-value field) single-value?
		(field single-value-field))
	
	(define instance (single-value 1))
	(single-value-field (typeless-cell instance))))

(define-test "constructing record type with one typeless uninitialized immutable field" (expect #!unit
	(define-record-type <single-value> (single-value) single-value?
		(field single-value-field))
	
	(define instance (single-value))
	(single-value-field instance)))

(define-test "constructing record type with one typed immutable field" (expect "Test string"
	(import (llambda typed))

	(define-record-type: <single-value> (single-value field) single-value?
		((field : <string>) single-value-field))
	
	(single-value-field (single-value "Test string"))))

(define-test "constructing record type with wrong type fails" (expect-failure
	(import (llambda typed))

	(define-record-type <single-value> (single-value field) single-value?
		((field : <string>) single-value-field))
	
	(single-value 50.5)))

(define-test "constructing record type with untyped mutable field" (expect "Test string"
	(define-record-type <mutable-value> (mutable-value) mutable-value?
		(field mutable-value-field set-mutable-value-field!))
	
	(define instance (mutable-value))
	(set-mutable-value-field! instance "Test string")
	(mutable-value-field instance)))

(define-test "constructing record type with typed mutable field" (expect -20
	(import (llambda typed))

	(define-record-type: <mutable-value> (mutable-value field) mutable-value?
		((field : <integer>) mutable-value-field set-mutable-value-field!))
	
	(define instance (mutable-value 50))
	(set-mutable-value-field! instance -20)
	(mutable-value-field instance)))

(define-test "mutating field with wrong type fails" (expect-failure
	(import (llambda typed))

	(define-record-type: <mutable-value> (mutable-value field) mutable-value?
		((field : <integer>) mutable-value-field set-mutable-value-field!))
	
	(set-mutable-value-field! (mutable-value 50) #t)))

(define-test "constructing record type with multiple fields" (expect (20 40 50.0)
	(import (llambda typed))

	; Use at least three 64bit fields to force out-of-line storage
	(define-record-type: <three-value> (three-value field1 field2 field3) three-value?
		((field1 : <integer>) three-value-field1)
		(field2 three-value-field2 set-three-value-field2!)
		((field3 : <flonum>) three-value-field3 set-three-value-field3!))
	
	(define instance (three-value 20 30 50.0))
	(set-three-value-field2! instance 40)

	(list (three-value-field1 instance) (three-value-field2 instance) (three-value-field3 instance))))

(define-test "constructors, accessors and mutators be boxed and invoked" (expect (20 40)
	(import (llambda typed))
	(import (llambda test-util))

	(define-record-type: <two-value> (two-value field1 field2) two-value?
		((field1 : <integer>) two-value-field1)
		(field2 two-value-field2 set-two-value-field2!))
	
	(define instance ((typeless-cell two-value) 20 30))
	((typeless-cell set-two-value-field2!) instance 40)

	(list ((typeless-cell two-value-field1) instance) ((typeless-cell two-value-field2) instance))))

; On 64bit we'll reorder these fields to fit inside the record's cell
(define-test "possibly repacked record type" (expect (#\a 2 #\c)
  (import (llambda typed))

	(define-record-type: <single-value> (single-value field1 field2 field3) single-value?
		((field1 : <char>) single-value-field1)
		((field2 : <integer>) single-value-field2)
		((field3 : <char>) single-value-field3))

	(define instance (single-value #\a 2 #\c))

	(list (single-value-field1 instance) (single-value-field2 instance) (single-value-field3 instance))))

(define-test "nested record types" (expect it-actually-worked
  (import (llambda typed))

	(define-record-type <inner-type> (inner-type field) inner-type?
		(field inner-type-field))
	
	(define-record-type: <outer-type> (outer-type inner-instance) outer-type?
		((inner-instance : <inner-type>) outer-type-inner-instance))

	(define instance (outer-type (inner-type 'it-actually-worked)))
	(inner-type-field (outer-type-inner-instance instance))))

