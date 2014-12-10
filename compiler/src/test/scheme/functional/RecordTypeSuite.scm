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

	(define-record-type <single-value> (single-value field) single-value?
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

	(define-record-type <mutable-value> (mutable-value field) mutable-value?
		((field : <exact-integer>) mutable-value-field set-mutable-value-field!))
	
	(define instance (mutable-value 50))
	(set-mutable-value-field! instance -20)
	(mutable-value-field instance)))

(define-test "mutating field with wrong type fails" (expect-failure
	(import (llambda typed))

	(define-record-type <mutable-value> (mutable-value field) mutable-value?
		((field : <exact-integer>) mutable-value-field set-mutable-value-field!))
	
	(set-mutable-value-field! (mutable-value 50) #t)))

(define-test "constructing record type with multiple fields" (expect (20 40 50.0)
	(import (llambda typed))

	; Use at least three 64bit fields to force out-of-line storage
	(define-record-type <three-value> (three-value field1 field2 field3) three-value?
		((field1 : <exact-integer>) three-value-field1)
		(field2 three-value-field2 set-three-value-field2!)
		((field3 : <flonum>) three-value-field3 set-three-value-field3!))
	
	(define instance (three-value 20 30 50.0))
	(set-three-value-field2! instance 40)

	(list (three-value-field1 instance) (three-value-field2 instance) (three-value-field3 instance))))

(define-test "constructors, accessors and mutators be boxed and invoked" (expect (20 40)
	(import (llambda typed))

	(define-record-type <two-value> (two-value field1 field2) two-value?
		((field1 : <exact-integer>) two-value-field1)
		(field2 two-value-field2 set-two-value-field2!))
	
	(define instance ((typeless-cell two-value) 20 30))
	((typeless-cell set-two-value-field2!) instance 40)

	(list ((typeless-cell two-value-field1) instance) ((typeless-cell two-value-field2) instance))))

; On 64bit we'll reorder these fields to fit inside the record's cell
(define-test "possibly repacked record type" (expect (#\a 2 #\c)
  (import (llambda typed))

	(define-record-type <single-value> (single-value field1 field2 field3) single-value?
		((field1 : <char>) single-value-field1)
		((field2 : <exact-integer>) single-value-field2)
		((field3 : <char>) single-value-field3))

	(define instance (single-value #\a 2 #\c))

	(list (single-value-field1 instance) (single-value-field2 instance) (single-value-field3 instance))))

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
		((world : <exact-integer>) single-value-field2))

  (world-value 5)
  ((typeless-cell world-value) 5)))

(define-test "record type inheritance" (expect-success
  (import (llambda typed))

  (define-record-type <named-number> (named-number name) named-number?
                      ([name : <string>] named-number-name set-named-number-name!))

  (define-record-type <named-int> <named-number> (named-int name value) named-int?
                      ([value : <exact-integer>] named-int-value set-named-int-value!))

  (define-record-type <named-flonum> <named-number> (named-flonum name value) named-flonum?
                      ([value : <flonum>] named-flonum-value set-named-flonum-value!))

  (define-record-type <named-nan> <named-flonum> (named-nan name value) named-nan?)

  (define test-named-number (named-number "Base"))
  (define test-named-int    (named-int    "Five" 5))
  (define test-named-flonum (named-flonum "Two" 2.0))
  (define test-named-nan    (named-nan    "NaN" +nan.0))

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
                     (assert-raises error-object? (accessor (typeless-cell value)))))))

  (assert-has-type named-number? test-named-number)
  (assert-has-type named-number? test-named-int)
  (assert-has-type named-number? test-named-flonum)
  (assert-has-type named-number? test-named-nan)

  (assert-not-type named-int? test-named-number)
  (assert-has-type named-int? test-named-int)
  (assert-not-type named-int? test-named-flonum)
  (assert-not-type named-int? test-named-nan)

  (assert-not-type named-flonum? test-named-number)
  (assert-not-type named-flonum? test-named-int)
  (assert-has-type named-flonum? test-named-flonum)
  (assert-has-type named-flonum? test-named-nan)

  (assert-not-type named-nan? test-named-number)
  (assert-not-type named-nan? test-named-int)
  (assert-not-type named-nan? test-named-flonum)
  (assert-has-type named-nan? test-named-nan)

  (assert-field-value "Base" named-number-name test-named-number)
  (assert-field-value "Five" named-number-name test-named-int)
  (assert-field-value "Two"  named-number-name test-named-flonum)
  (assert-field-value "NaN"  named-number-name test-named-nan)

  (assert-lacks-field named-int-value test-named-number)
  (assert-field-value 5 named-int-value test-named-int)
  (assert-lacks-field named-int-value test-named-flonum)
  (assert-lacks-field named-int-value test-named-nan)

  (assert-lacks-field named-flonum-value test-named-number)
  (assert-lacks-field named-flonum-value test-named-int)
  (assert-field-value 2.0 named-flonum-value test-named-flonum)
  (assert-field-value +nan.0 named-flonum-value test-named-nan)

  (set-named-number-name! test-named-number "Number")
  (set-named-number-name! test-named-int "Int")
  (set-named-number-name! test-named-flonum "Flonum")

  (assert-field-value "Number" named-number-name test-named-number)
  (assert-field-value "Int" named-number-name test-named-int)
  (assert-field-value "Flonum"  named-number-name test-named-flonum)
  (assert-field-value "NaN"  named-number-name test-named-nan)))

(define-test "recursive record types" (expect-success
  (import (llambda typed))

  (define-record-type <dodgy-list> (dcons dcar dcdr) dodgy-list?
                      ([dcar : <any>] dcar set-dcar!)
                      ([dcdr : (U <dodgy-list> '())] dcdr set-dcdr!))

  (define dodgy-list (dcons 1 (dcons 2 (dcons 3 (dcons 4 '())))))

  (assert-equal 1 (dcar dodgy-list))
  (assert-equal 2 (dcar (dcdr dodgy-list)))
  (assert-equal 3 (dcar (dcdr (dcdr dodgy-list))))
  (assert-equal 4 (dcar (dcdr (dcdr (dcdr dodgy-list)))))
  (assert-equal '() (dcdr (dcdr (dcdr (dcdr dodgy-list)))))

  (assert-raises error-object?
    (set-dcdr! dodgy-list (typeless-cell #f)))

  (set-dcdr! dodgy-list '())
  (assert-equal '() (dcdr dodgy-list))))

(define-test "polymorphoc record types" (expect-success
  (import (llambda typed))

  (define-record-type (Some A) (some val) some?
                      ([val : A] some-value))

  (define-record-type (None) (none) none?)

  (define-type (Option A) (U (Some A) (None)))

  (define some-string (some "Hello, world!"))
  (ann some-string (Some <string>))

  (define string-value (some-value some-string))
  (ann string-value <string>)

  (define-record-type (CustomPair A B) (ccons ccar ccdr) custom-pair?
                      ([ccar : A] ccar)
                      ([ccdr : B] ccdr))

  (define custom-list (ccons 1 (ccons 'two (ccons 3.0 '()))))
  (ann custom-list (CustomPair <exact-integer> (CustomPair <symbol> (CustomPair <flonum> '()))))

  (ann (ccar custom-list) <exact-integer>)
  (ann (ccdr custom-list) (CustomPair <symbol> (CustomPair <flonum> '())))

  (ann (ccar (ccdr custom-list)) <symbol>)
  (ann (ccdr (ccdr custom-list)) (CustomPair <flonum> '()))

  (ann (ccar (ccdr (ccdr custom-list))) <flonum>)
  (ann (ccdr (ccdr (ccdr custom-list))) '())))

(define-test "inheriting polymorphic record types" (expect-success
  (import (llambda typed))

  (define-record-type (Some A) (some val) some?
                      ([val : A] some-value))

  (define-record-type <some-string> (Some <string>) (some-string val) some-string?)

  (define string-opt (some-string "Hello, world!"))

  ; We are of our derived instance
  (ann string-opt <some-string>)
  ; And our parent instance
  (ann string-opt (Some <string>))
  ; Using our parent accessor returns the correct type
  (ann (some-value string-opt) <string>)))

(define-test "passing polymorphic record types to procedures" (expect-success
  (import (llambda typed))

  (define-record-type (Some A) (some val) some?
                      ([val : A] some-value))

  (define-record-type (None) (none) none?)

  (define-type (Option A) (U (Some A) (None)))

  (define (some-string-or-empty [string-opt : (Option <string>)])
    (if (none? string-opt)
      ""
      (some-value string-opt)))

  (assert-equal "Hello!" (some-string-or-empty (some "Hello!")))
  (assert-equal "" (some-string-or-empty (none)))))

(define-test "attempted dynamic checks for polymorphic record type fail at compile time" (expect-compile-failure
  (import (llambda typed))

  (define-record-type (Some A) (some val) some?
                      ([val : A] some-value))

  (define-record-type (None) (none) none?)

  (define-type (Option A) (U (Some A) (None)))

  (define (some-string-or-empty [string-opt : (Option <string>)])
    (if (none? string-opt)
      ""
      (some-value string-opt)))

  (define any-option (typed-dynamic (some "Hello!") (Option <any>)))
  (some-string-or-empty any-option)))

(define-test "passing polymorphic record type to polymorphic procedure" (expect-success
  (import (llambda typed))

  (define-record-type (Some A) (some val) some?
                      ([val : A] some-value))

  (define-record-type (None) (none) none?)

  (define-type (Option A) (U (Some A) (None)))

  (: none-to-false (All (A) (Option A) (U A #f)))
  (define (none-to-false opt)
    (if (none? opt)
      #f
      (some-value opt)))

  (define dynamic-option (typed-dynamic (some "Hello!") (Option <string>)))

  (ann (none-to-false dynamic-option) (U #f <string>))))

(define-test "polymorphic record type containing procedure" (expect-success
  (import (llambda typed))

  (define-record-type (Some A) (some val) some?
                      ([val : A] some-value))

  (define some-minus (some -))

  (assert-equal 2.0 ((some-value some-minus) 7.0 2 3))))

(define-test "polymorphic record type containing procedure passed to polymorpic procedure" (expect-success
  (import (llambda typed))

  (define-record-type (ValueThunk A B) (value-thunk val proc) value-thunk?
                      ([val : A] value-thunk-value)
                      ([proc : (-> A B)] value-thunk-proc))

  (: apply-value-thunk (All (A B) (ValueThunk A B) B))
  (define (apply-value-thunk thunk)
    ((value-thunk-proc thunk) (value-thunk-value thunk)))

  (define string-value-thunk (value-thunk "Hello" string->symbol))
  (ann string-value-thunk (ValueThunk <string> <symbol>))

  (ann (apply-value-thunk string-value-thunk) <symbol>)
  (assert-equal 'Hello (apply-value-thunk string-value-thunk))))

(define-test "polymorphic record type with upper type bounds" (expect-success
  (import (llambda typed))

  (define-record-type (NumberedRecord [A : <number>] [B : <exact-integer>]) (numbered-record num-val int-val) numbered-record?
                      ([num-val : A] numbered-record-num-val)
                      ([int-val : B] numbered-record-int-val))

  (define num-record (numbered-record 5.0 -1))
  (ann num-record (NumberedRecord <flonum> <exact-integer>))
  (ann (numbered-record-num-val num-record) <flonum>)
  (ann (numbered-record-int-val num-record) <exact-integer>)))

(define-test "constructing polymorphic record type with violated type bounds fails at compile time" (expect-compile-failure
  (import (llambda typed))

  (define-record-type (NumberedRecord [A : <number>] [B : <exact-integer>]) (numbered-record num-val int-val) numbered-record?
                      ([num-val : A] numbered-record-num-val)
                      ([int-val : B] numbered-record-int-val))

  (define num-record (numbered-record 5.0 5.0))))
