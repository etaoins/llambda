(define-test "constructing empty record type" (expect #t
  (define-record-type <empty> (empty) empty?)
  (empty? (empty))))

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

(define-test "constructing record type with one typeless initialized immutable field" (expect-static-success
  (define-record-type <single-value> (single-value field) single-value?
    (field single-value-field))

  (define instance (single-value 1))
  (assert-equal 1 (single-value-field instance))))

(define-test "record types can be converted to and from typeless data" (expect 1
  (define-record-type <single-value> (single-value field) single-value?
    (field single-value-field))

  (define instance (single-value 1))
  (single-value-field (typeless-cell instance))))

(define-test "constructing record type with one typeless uninitialized immutable field" (expect #!unit
  (define-record-type <single-value> (single-value) single-value?
    (field single-value-field))

  (define instance (single-value))
  (single-value-field instance)))

(define-test "constructing record type with one typed immutable field" (expect-static-success
  (import (llambda typed))

  (define-record-type <single-value> (single-value field) single-value?
    ((field : <string>) single-value-field))

  (assert-equal "Test string" (single-value-field (single-value "Test string")))))

(define-test "constructing record type with wrong type fails" (expect-error type-error?
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

  (define-record-type <mutable-value> (mutable-value field) mutable-value?
    ((field : <integer>) mutable-value-field set-mutable-value-field!))

  (define instance (mutable-value 50))
  (set-mutable-value-field! instance -20)
  (mutable-value-field instance)))

(define-test "mutating field with wrong type fails" (expect-error type-error?
  (import (llambda typed))

  (define-record-type <mutable-value> (mutable-value field) mutable-value?
    ((field : <integer>) mutable-value-field set-mutable-value-field!))

  (set-mutable-value-field! (mutable-value 50) #t)))

(define-test "constructing record type with multiple fields" (expect (20 40 50.0)
  (import (llambda typed))

  ; Use at least three 64bit fields to force out-of-line storage
  (define-record-type <three-value> (three-value field1 field2 field3) three-value?
    ((field1 : <integer>) three-value-field1)
    (field2 three-value-field2 set-three-value-field2!)
    ((field3 : <flonum>) three-value-field3 set-three-value-field3!))

  (define instance (three-value 20 30 50.0))
  (set-three-value-field2! instance 40)

  (list (three-value-field1 instance) (three-value-field2 instance) (three-value-field3 instance))))

(define-test "constructors, accessors and mutators be boxed and invoked" (expect (20 40)
  (import (llambda typed))

  (define-record-type <two-value> (two-value field1 field2) two-value?
    ((field1 : <integer>) two-value-field1)
    (field2 two-value-field2 set-two-value-field2!))

  (define instance ((typeless-cell two-value) 20 30))
  ((typeless-cell set-two-value-field2!) instance 40)

  (list ((typeless-cell two-value-field1) instance) ((typeless-cell two-value-field2) instance))))

; On 64bit we'll reorder these fields to fit inside the record's cell
(define-test "possibly repacked record type" (expect (#\a 2 #\c)
  (import (llambda typed))

  (define-record-type <single-value> (single-value field1 field2 field3) single-value?
    ((field1 : <char>) single-value-field1)
    ((field2 : <integer>) single-value-field2)
    ((field3 : <char>) single-value-field3))

  (define instance (single-value #\a 2 #\c))

  (list (single-value-field1 instance) (single-value-field2 instance) (single-value-field3 instance))))

(define-test "constant out-of-line record" (expect-static-success
  (import (llambda typed))

  (define-record-type <single-value> (single-value field1 field2 field3) single-value?
    ((field1 : <integer>) single-value-field1)
    ((field2 : <integer>) single-value-field2)
    ((field3 : <integer>) single-value-field3))

  (define instance (single-value 1 2 3))

  (assert-equal 1 (single-value-field1 instance))
  (assert-equal 2 (single-value-field2 instance))
  (assert-equal 3 (single-value-field3 instance))))

(define-test "nested record types" (expect it-actually-worked
  (import (llambda typed))

  (define-record-type <inner-type> (inner-type field) inner-type?
    (field inner-type-field))

  (define-record-type <outer-type> (outer-type inner-instance) outer-type?
    ((inner-instance : <inner-type>) outer-type-inner-instance))

  (define instance (outer-type (inner-type 'it-actually-worked)))
  (inner-type-field (outer-type-inner-instance instance))))

(define-test "record type with special field names" (expect-success
  (import (llambda typed))

  (define-record-type <world-value> (world-value world) world-value?
    ((world : <integer>) single-value-field2))

  (world-value 5)
  ((typeless-cell world-value) 5)))

(define-test "record type inheritance" (expect-success
  (import (llambda typed))
  (import (llambda error))

  (define-record-type <named-number> (named-number name) named-number?
                      ([name : <string>] named-number-name set-named-number-name!))

  (define-record-type <named-int> <named-number> (named-int name value) named-int?
                      ([value : <integer>] named-int-value set-named-int-value!))

  (define-record-type <named-flonum> <named-number> (named-flonum name value) named-flonum?
                      ([value : <flonum>] named-flonum-value set-named-flonum-value!))

  (define-record-type <named-nan> <named-flonum> (named-nan name value) named-nan?)

  ; This is intentionally out-of-line
  (define-record-type <named-complex> <named-flonum> (named-complex name value mag ang) named-complex?
                      ([mag : <flonum>] named-complex-mag set-named-complex-mag!)
                      ([ang : <flonum>] named-complex-ang set-named-complex-ang!))

  (define test-named-number  (named-number  "Base"))
  (define test-named-int     (named-int     "Five" 5))
  (define test-named-flonum  (named-flonum  "Two" 2.0))
  (define test-named-nan     (named-nan     "NaN" +nan.0))
  (define test-named-complex (named-complex "Complex" 3.0 4.0 5.0))

  (define-syntax assert-has-type
    (syntax-rules ()
                  ((assert-has-type pred? value)
                   (begin
                     (assert-true (pred? value))
                     (assert-true (pred? (typeless-cell value)))))))

  (define-syntax assert-not-type
    (syntax-rules ()
                  ((assert-not-type pred? value)
                   (begin
                     (assert-false (pred? value))
                     (assert-false (pred? (typeless-cell value)))))))

  (define-syntax assert-field-value
    (syntax-rules ()
                  ((assert-field-value expected accessor value)
                   (begin
                     (assert-equal expected (accessor value))
                     (assert-equal expected (accessor (typeless-cell value)))))))

  (define-syntax assert-lacks-field
    (syntax-rules ()
                  ((assert-lacks-field accessor value)
                   (begin
                     (assert-raises type-error? (accessor (typeless-cell value)))))))

  (assert-has-type named-number? test-named-number)
  (assert-has-type named-number? test-named-int)
  (assert-has-type named-number? test-named-flonum)
  (assert-has-type named-number? test-named-nan)
  (assert-has-type named-number? test-named-complex)

  (assert-not-type named-int? test-named-number)
  (assert-has-type named-int? test-named-int)
  (assert-not-type named-int? test-named-flonum)
  (assert-not-type named-int? test-named-nan)
  (assert-not-type named-int? test-named-complex)

  (assert-not-type named-flonum? test-named-number)
  (assert-not-type named-flonum? test-named-int)
  (assert-has-type named-flonum? test-named-flonum)
  (assert-has-type named-flonum? test-named-nan)
  (assert-has-type named-flonum? test-named-complex)

  (assert-not-type named-nan? test-named-number)
  (assert-not-type named-nan? test-named-int)
  (assert-not-type named-nan? test-named-flonum)
  (assert-has-type named-nan? test-named-nan)
  (assert-not-type named-nan? test-named-complex)

  (assert-not-type named-complex? test-named-number)
  (assert-not-type named-complex? test-named-int)
  (assert-not-type named-complex? test-named-flonum)
  (assert-not-type named-complex? test-named-nan)
  (assert-has-type named-complex? test-named-complex)

  (assert-field-value "Base"     named-number-name test-named-number)
  (assert-field-value "Five"     named-number-name test-named-int)
  (assert-field-value "Two"      named-number-name test-named-flonum)
  (assert-field-value "NaN"      named-number-name test-named-nan)
  (assert-field-value "Complex"  named-number-name test-named-complex)

  (assert-lacks-field named-int-value test-named-number)
  (assert-field-value 5 named-int-value test-named-int)
  (assert-lacks-field named-int-value test-named-flonum)
  (assert-lacks-field named-int-value test-named-nan)
  (assert-lacks-field named-int-value test-named-complex)

  (assert-lacks-field named-flonum-value test-named-number)
  (assert-lacks-field named-flonum-value test-named-int)
  (assert-field-value 2.0 named-flonum-value test-named-flonum)
  (assert-field-value +nan.0 named-flonum-value test-named-nan)
  (assert-field-value 3.0 named-flonum-value test-named-complex)

  (set-named-number-name! test-named-number "Number")
  (set-named-number-name! test-named-int "Int")
  (set-named-number-name! test-named-flonum "Flonum")

  (assert-field-value "Number"   named-number-name test-named-number)
  (assert-field-value "Int"      named-number-name test-named-int)
  (assert-field-value "Flonum"   named-number-name test-named-flonum)
  (assert-field-value "NaN"      named-number-name test-named-nan)
  (assert-field-value "Complex"  named-number-name test-named-complex)))

(define-test "constant out-of-line record inheritance" (expect-success
  (import (llambda typed))

  (define-record-type <base-record> (base-record field1 field2) base-record?
                      ([field1 : <integer>] base-record-field1)
                      ([field2 : <integer>] base-record-field2))

  (define-record-type <child-record> <base-record> (child-record field1 field2 field3 field4) child-record?
                      ([field3 : <integer>] child-record-field3)
                      ([field4 : <integer>] child-record-field4))

  (define instance (child-record 1 2 3 4))

  (assert-equal 1 (base-record-field1 instance))
  (assert-equal 2 (base-record-field2 instance))
  (assert-equal 3 (child-record-field3 instance))
  (assert-equal 4 (child-record-field4 instance))

  (assert-equal 1 (base-record-field1 (typeless-cell instance)))
  (assert-equal 2 (base-record-field2 (typeless-cell instance)))
  (assert-equal 3 (child-record-field3 (typeless-cell instance)))
  (assert-equal 4 (child-record-field4 (typeless-cell instance)))))

(define-test "recursive record types" (expect-success
  (import (llambda typed))
  (import (llambda error))

  (define-record-type <dodgy-list> (dcons dcar dcdr) dodgy-list?
                      ([dcar : <any>] dcar set-dcar!)
                      ([dcdr : (U <dodgy-list> '())] dcdr set-dcdr!))

  (define dodgy-list (dcons 1 (dcons 2 (dcons 3 (dcons 4 '())))))

  (assert-equal 1 (dcar dodgy-list))
  (assert-equal 2 (dcar (dcdr dodgy-list)))
  (assert-equal 3 (dcar (dcdr (dcdr dodgy-list))))
  (assert-equal 4 (dcar (dcdr (dcdr (dcdr dodgy-list)))))
  (assert-equal '() (dcdr (dcdr (dcdr (dcdr dodgy-list)))))

  (assert-raises type-error?
    (set-dcdr! dodgy-list (typeless-cell #f)))

  (set-dcdr! dodgy-list '())
  (assert-equal '() (dcdr dodgy-list))))

(define-test "external record types with predicate function" (expect-success
  ; <poison-pill> is provided by the actor library
  (import (llambda actor))

  (define-record-type <other-record> (other-record) other-record?)

  (define other (other-record))
  (define pill (poison-pill-object))

  (assert-true  (poison-pill-object? pill))
  (assert-false (poison-pill-object? '()))
  (assert-false (poison-pill-object? other))
  (assert-false (other-record? pill))

  (assert-true  (poison-pill-object? (typeless-cell pill)))
  (assert-false (poison-pill-object? (typeless-cell '())))
  (assert-false (poison-pill-object? (typeless-cell other)))
  (assert-false (other-record? (typeless-cell pill)))))

(define-test "external record types without predicate functions do not support dynamic type checks" (expect-compile-error type-error?
  (import (llambda typed))

  (define-type <external-record> (ExternalRecord))
  (define-predicate external-record? <external-record>)

  (external-record? (typeless-cell '()))))

