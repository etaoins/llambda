(define-test "constructing empty record type" (expect #t
	(define-record-type <unit> (unit) unit?)
	(unit? (unit))))

(define-test "record types are disjoint" (expect #f
	(define-record-type <type1> (type1) type1?)
	(define-record-type <type2> (type2) type2?)
	(type1? (type2))))

(define-test "constructing record type with one typeless initialized immutable field" (expect 1
	(define-record-type <single-value> (single-value field) single-value?
		(field single-value-field))
	
	(define instance (single-value 1))
	(single-value-field instance)))

(define-test "record types can be converted to and from boxed data" (expect 1
	(import (llambda test-util))

	(define-record-type <single-value> (single-value field) single-value?
		(field single-value-field))
	
	(define instance (single-value 1))
	(single-value-field (typeless-boxed instance))))

(define-test "constructing record type with one typeless uninitialized immutable field" (expect #!unspecific
	(define-record-type <single-value> (single-value) single-value?
		(field single-value-field))
	
	(define instance (single-value))
	(single-value-field instance)))

(define-test "constructing record type with one typed immutable field" (expect "Test string"
	(import (llambda nfi))

	(define-record-type <single-value> (single-value field) single-value?
		((field : <boxed-string>) single-value-field))
	
	(single-value-field (single-value "Test string"))))

(define-test "constructing record type with wrong type fails" (expect-failure
	(import (llambda nfi))

	(define-record-type <single-value> (single-value field) single-value?
		((field : <boxed-string>) single-value-field))
	
	(single-value 50.5)))

(define-test "constructing record type with untyped mutable field" (expect "Test string"
	(define-record-type <mutable-value> (mutable-value) mutable-value?
		(field mutable-value-field set-mutable-value-field!))
	
	(define instance (mutable-value))
	(set-mutable-value-field! instance "Test string")
	(mutable-value-field instance)))

(define-test "constructing record type with typed mutable field" (expect -20
	(import (llambda nfi))

	(define-record-type <mutable-value> (mutable-value field) mutable-value?
		((field : <int64>) mutable-value-field set-mutable-value-field!))
	
	(define instance (mutable-value 50))
	(set-mutable-value-field! instance -20)
	(mutable-value-field instance)))

(define-test "mutating field with wrong type fails" (expect-failure
	(import (llambda nfi))

	(define-record-type <mutable-value> (mutable-value field) mutable-value?
		((field : <int64>) mutable-value-field set-mutable-value-field!))
	
	(set-mutable-value-field! (mutable-value 50) #t)))

(define-test "constructing record type with multiple fields" (expect (20 40)
	(import (llambda nfi))

	(define-record-type <two-value> (two-value field1 field2) two-value?
		((field1 : <int64>) two-value-field1)
		(field2 two-value-field2 set-two-value-field2!))
	
	(define instance (two-value 20 30))
	(set-two-value-field2! instance 40)

	(list (two-value-field1 instance) (two-value-field2 instance))))

(define-test "nested record types" (expect 'it-actually-worked
	(define-record-type <inner-type> (inner-type field) inner-type?
		(field inner-type-field))
	
	(define-record-type <outer-type> (outer-type inner-instance) outer-type?
		((inner-instance : <inner-type>) outer-type-inner-instance))

	(define instance (outer-type (inner-type 'it-actually-worked)))
	(inner-type-field (outer-type-inner-instance instance))))

