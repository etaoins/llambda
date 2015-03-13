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

(define-test "(define-predicate) for pairs" (expect-static-success
  (import (llambda typed))

  (define-predicate any-pair? (Pairof <any> <any>))
  (define-predicate string-symbol-pair? (Pairof <string> <symbol>))
  (define-predicate symbol-string-pair? (Pairof <symbol> <string>))

  (assert-true  (any-pair? '(1 . 2)))
  (assert-true  (any-pair? '(foo . "bar")))
  (assert-true  (any-pair? '("bar" . foo)))

  (assert-false (string-symbol-pair? '(1 . 2)))
  (assert-false (string-symbol-pair? '(foo . "bar")))
  (assert-true  (string-symbol-pair? '("bar" . foo)))

  (assert-false (symbol-string-pair? '(1 . 2)))
  (assert-true  (symbol-string-pair? '(foo . "bar")))
  (assert-false (symbol-string-pair? '("bar" . foo)))

  (define-predicate two-number-proper-list? (Pairof <number> (Pairof <number> <empty-list>)))

  (assert-true (two-number-proper-list? '(1 2)))
  (assert-true (two-number-proper-list? '(1.0 -5)))
  (assert-false (two-number-proper-list? '(1 . 2)))
  (assert-false (two-number-proper-list? '(1 sneaky-symbol)))))

(define-test "(define-predicate) for lists" (expect-static-success
  (import (llambda typed))

  (define-predicate string-list? (Listof <string>))
  (define-predicate symbol-list? (Listof <symbol>))

  (assert-true  (string-list? '("one" "two")))
  (assert-false (string-list? '(one two)))
  (assert-false (string-list? '(1 2)))

  (assert-false (symbol-list? '("one" "two")))
  (assert-true  (symbol-list? '(one two)))
  (assert-false (symbol-list? '(1 2)))))

(define-test "(define-predicate) for uniform vectors" (expect-success
  (import (llambda typed))
  (import (only (llambda internal primitives) Vectorof)) ; (Vectorof is internal)

  (define number-vector #(1 2 3.5))
  (define exact-int-vector #(1 2 3))
  (define mixed-vector #(1 'a #f))
  (define empty-vector #())
  (define untyped-number-vector (typeless-cell number-vector))
  (define untyped-exact-int-vector (typeless-cell exact-int-vector))
  (define untyped-mixed-vector (typeless-cell mixed-vector))
  (define untyped-empty-vector (typeless-cell empty-vector))

  (define-predicate number-vector? (Vectorof <number>))
  (define-predicate exact-int-vector? (Vectorof <exact-integer>))

  (assert-true  (number-vector? number-vector))
  (assert-true  (number-vector? exact-int-vector))
  (assert-false (number-vector? mixed-vector))
  (assert-true  (number-vector? empty-vector))
  (assert-true  (number-vector? untyped-number-vector))
  (assert-true  (number-vector? untyped-exact-int-vector))
  (assert-false (number-vector? untyped-mixed-vector))
  (assert-true  (number-vector? untyped-empty-vector))

  (assert-false (exact-int-vector? number-vector))
  (assert-true  (exact-int-vector? exact-int-vector))
  (assert-false (exact-int-vector? mixed-vector))
  (assert-true  (exact-int-vector? empty-vector))
  (assert-false (exact-int-vector? untyped-number-vector))
  (assert-true  (exact-int-vector? untyped-exact-int-vector))
  (assert-false (exact-int-vector? untyped-mixed-vector))
  (assert-true  (exact-int-vector? untyped-empty-vector))))

(define-test "(define-predicate) for specific vectors" (expect-success
  (import (llambda typed))
  (import (only (llambda internal primitives) Vector)) ; (Vector is internal)

  (define symbol-string-vector #(symbol "string"))
  (define symbol-string-string-vector #(symbol "string" "string"))
  (define exact-int-vector #(1 2 3))
  (define empty-vector #())
  (define untyped-symbol-string-vector (typeless-cell symbol-string-vector))
  (define untyped-symbol-string-string-vector (typeless-cell symbol-string-string-vector))
  (define untyped-exact-int-vector (typeless-cell exact-int-vector))
  (define untyped-empty-vector (typeless-cell empty-vector))

  (define-predicate symbol-string-vector? (Vector <symbol> <string>))
  (define-predicate symbol-string-string-vector? (Vector <symbol> <string> <string>))
  (define-predicate empty-vector? (Vector))

  (assert-true  (symbol-string-vector? symbol-string-vector))
  (assert-false (symbol-string-vector? symbol-string-string-vector))
  (assert-false (symbol-string-vector? exact-int-vector))
  (assert-false (symbol-string-vector? empty-vector))
  (assert-true  (symbol-string-vector? untyped-symbol-string-vector))
  (assert-false (symbol-string-vector? untyped-symbol-string-string-vector))
  (assert-false (symbol-string-vector? untyped-exact-int-vector))
  (assert-false (symbol-string-vector? untyped-empty-vector))

  (assert-false (symbol-string-string-vector? symbol-string-vector))
  (assert-true  (symbol-string-string-vector? symbol-string-string-vector))
  (assert-false (symbol-string-string-vector? exact-int-vector))
  (assert-false (symbol-string-string-vector? empty-vector))
  (assert-false (symbol-string-string-vector? untyped-symbol-string-vector))
  (assert-true  (symbol-string-string-vector? untyped-symbol-string-string-vector))
  (assert-false (symbol-string-string-vector? untyped-exact-int-vector))
  (assert-false (symbol-string-string-vector? untyped-empty-vector))

  (assert-false (empty-vector? symbol-string-vector))
  (assert-false (empty-vector? symbol-string-string-vector))
  (assert-false (empty-vector? exact-int-vector))
  (assert-true  (empty-vector? empty-vector))
  (assert-false (empty-vector? untyped-symbol-string-vector))
  (assert-false (empty-vector? untyped-symbol-string-string-vector))
  (assert-false (empty-vector? untyped-exact-int-vector))
  (assert-true  (empty-vector? untyped-empty-vector))))

(define-test "(define-predicate) for vector trees" (expect-success
  (import (llambda typed))
  (import (only (llambda internal primitives) Vectorof)) ; (Vectorof is internal)

  (define symbol-vector-tree
    #(one #(two three #(four)) #()))

  (define mixed-vector-tree
    #(one #(two "three" #(four)) #()))

  (define untyped-symbol-vector-tree (typeless-cell symbol-vector-tree))
  (define untyped-mixed-vector-tree (typeless-cell mixed-vector-tree))

  (define-predicate symbol-vector-tree? (Rec VT (U <symbol> (Vectorof VT))))

  (assert-true  (symbol-vector-tree? symbol-vector-tree))
  (assert-false (symbol-vector-tree? mixed-vector-tree))

  (assert-true  (symbol-vector-tree? untyped-symbol-vector-tree))
  (assert-false (symbol-vector-tree? untyped-mixed-vector-tree))))

(define-test "(define-predicate) for binary trees" (expect-success
  (import (llambda typed))

  (define-predicate string-tree? (Rec BT (U <string> (Pairof BT BT))))

  (define string-list '("one" "two"))
  (define bare-string "one")
  (define string-tree '("one" . ("three" . "four")))
  (define untyped-string-tree (typeless-cell string-tree))
  (define untyped-string-list (typeless-cell string-list))

  (define symbol-list '(one two))
  (define bare-symbol 'one)
  (define symbol-tree '(one . (three . four)))
  (define untyped-symbol-tree (typeless-cell symbol-tree))
  (define untyped-symbol-list (typeless-cell symbol-list))

  (assert-false (string-tree? string-list))
  (assert-true  (string-tree? bare-string))
  (assert-true  (string-tree? string-tree))
  (assert-true  (string-tree? untyped-string-tree))
  (assert-false (string-tree? untyped-string-list))

  (assert-false (string-tree? symbol-list))
  (assert-false (string-tree? bare-symbol))
  (assert-false (string-tree? symbol-tree))
  (assert-false (string-tree? untyped-symbol-tree))
  (assert-false (string-tree? untyped-symbol-list))))

(define-test "(define-predicate) for associative lists" (expect-success
  (import (llambda typed))

  (define symbol-list '(one two))
  (define bare-symbol 'one)
  (define symbol-to-int-alist '((one 1) (two 2) (three 3)))
  (define string-to-int-alist '(("one" 1) ("two" 2) ("three" 3)))

  (define-predicate symbol-to-int-alist? (Assocof <symbol> (Listof <exact-integer>)))

  (assert-false (symbol-to-int-alist? symbol-list))
  (assert-false (symbol-to-int-alist? bare-symbol))
  (assert-true  (symbol-to-int-alist? symbol-to-int-alist))
  (assert-false (symbol-to-int-alist? string-to-int-alist))))

(define-test "(define-predicate) for symbols" (expect-success
  (import (llambda typed))

  (define-predicate hello? 'hello)
  (define-predicate hello-or-goodbye? (U 'hello 'goodbye))

  (define hello-symbol 'hello)
  (define hello-string "hello")
  (define goodbye-symbol 'goodbye)

  (define untyped-hello-symbol (typeless-cell hello-symbol))
  (define untyped-hello-string (typeless-cell hello-string))
  (define untyped-goodbye-symbol (typeless-cell goodbye-symbol))

  (assert-true  (hello? hello-symbol))
  (assert-false (hello? hello-string))
  (assert-false (hello? goodbye-symbol))

  (assert-true  (hello? untyped-hello-symbol))
  (assert-false (hello? untyped-hello-string))
  (assert-false (hello? untyped-goodbye-symbol))

  (assert-true  (hello-or-goodbye? hello-symbol))
  (assert-false (hello-or-goodbye? hello-string))
  (assert-true  (hello-or-goodbye? goodbye-symbol))

  (assert-true  (hello-or-goodbye? untyped-hello-symbol))
  (assert-false (hello-or-goodbye? untyped-hello-string))
  (assert-true  (hello-or-goodbye? untyped-goodbye-symbol))))

(define-test "(define-predicate) for proper lists in unions" (expect-success
  (import (llambda typed))

  ; This is testing that we preserve recursive union structure when flattening them in to larger union
  (define-predicate int-list-or-false? (U (Listof <exact-integer>) #f))

  (define false #f)
  (define int-list '(1 2 3))
  (define improper-int-list '(1 2 . #f))

  (assert-true  (int-list-or-false? false))
  (assert-true  (int-list-or-false? int-list))
  (assert-false (int-list-or-false? improper-int-list))

  (define untyped-false (typeless-cell false))
  (define untyped-int-list (typeless-cell int-list))
  (define untyped-improper-int-list (typeless-cell improper-int-list))

  (assert-true  (int-list-or-false? untyped-false))
  (assert-true  (int-list-or-false? untyped-int-list))
  (assert-false (int-list-or-false? untyped-improper-int-list))))
