(define-test "applying mandatory arg storage locs constrains their type" (expect-success
  (import (llambda typed))
  (define typeless-vector (typeless-cell #(1 2 3)))
  (define typeless-1 (typeless-cell 1))

  (define ref-result (vector-ref typeless-vector typeless-1))

  (assert-equal #(1 2 3) (ann typeless-vector <vector>))
  (assert-equal 1 (ann typeless-1 <integer>))
  (assert-equal 2 ref-result)))

(define-test "applying optional arg storage locs constrains their type" (expect-success
  (import (llambda typed))
  (define typeless-1 (typeless-cell 1))

  (define (take-optional-number [val : <integer> 2]) val)
  (define take-result (take-optional-number typeless-1))

  (assert-equal 1 (ann typeless-1 <integer>))
  (assert-equal 1 take-result)))

(define-test "applying fixed arg storage locs to a case procedure constrains their type" (expect 4
  (import (scheme case-lambda))
  (import (llambda typed))

  (define typed-case (case-lambda
    (([first : <integer>]) first)
    (([first : <flonum>] [second : <integer>]) first)))

  (define first (typeless-cell 1.5))
  (define second (typeless-cell 4))

  ; This should match the second clause statically
  (typed-case first second)

  (ann first <flonum>)
  (ann second <integer>)))

(define-test "applying fixed arg storage locs through a typed procedure value constrains their type" (expect 2
  (import (llambda typed))
  (: apply-vector-proc (-> (-> <vector> <integer> <any>) <any> <any> <any>))
  (define (apply-vector-proc vector-proc vec offset)
    (define result (vector-proc vec offset))
    (ann vec <vector>)
    (ann offset <integer>)
    result)

  (apply-vector-proc vector-ref #(1 2 3) 1)))

(define-test "applying rest arg storage locs constrains their type" (expect-success
  (import (llambda typed))
  (define typeless-2 (typeless-cell 2))
  (define typeless-3 (typeless-cell 3))

  (define *-result (* typeless-2 typeless-3))

  (assert-equal 2 (ann typeless-2 <number>))
  (assert-equal 3 (ann typeless-3 <number>))
  (assert-equal 6 *-result)))

(define-test "applying rest arg storage locs through a typed procedure value constrains their type" (expect 6
  (import (llambda typed))
  (define typeless-2 (typeless-cell 2))
  (define typeless-3 (typeless-cell 3))

  (: apply-numbers-proc (-> (-> <number> * <number>) <any> <any> <number>))
  (define (apply-numbers-proc numbers-proc op1 op2)
    (define result (numbers-proc op1 op2))
    (ann op1 <number>)
    (ann op2 <number>)
    result)

  (apply-numbers-proc * 3 2)))

(define-test "applying a procedure constraints the procedure value's type" (expect-success
  (import (llambda typed))

  ; Test using the top procedure type (as part of <any>)
  (define typeless-proc (typeless-cell +))
  (assert-equal 6 (typeless-proc 1 2 3))
  (ann typeless-proc <procedure>)

  ; Test using a specific procedure type
  (define-type <expected-type> (-> <number> * <number>))

  (define partially-typed-proc (typed-dynamic + (U <boolean> <expected-type>)))
  (assert-equal 6 (partially-typed-proc 1 2 3))
  (ann partially-typed-proc <expected-type>)))

(define-test "(if) propagates test truthiness information to its branches" (expect-success
  (import (llambda typed))

  (define typeless-bool (cast (typeless-cell #f) <boolean>))

  (if typeless-bool
    (ann typeless-bool #t)
    (ann typeless-bool #f))))

(define-test "(not) propagates test truthiness information to branches" (expect-success
  (import (llambda typed))

  (define typeless-bool (cast (typeless-cell #f) <boolean>))

  (if (not typeless-bool)
    (ann typeless-bool #f)
    (ann typeless-bool #t))))

(define-test "(and) propagates test truthiness information to branches" (expect-success
  (import (llambda typed))

  (define typeless-bool1 (cast (typeless-cell #f) <boolean>))
  (define typeless-bool2 (cast (typeless-cell #f) <boolean>))

  (if (and typeless-bool1 typeless-bool2)
    (begin
      (ann typeless-bool2 #t)
      (ann typeless-bool2 #t)))))

(define-test "(when) propagates test truthiness information" (expect-success
  (import (llambda typed))

  (define typeless-bool (cast (typeless-cell #f) <boolean>))

  (when typeless-bool
    (ann typeless-bool #t))))

(define-test "(unless) propagates test truthiness information" (expect-success
  (import (llambda typed))

  (define typeless-bool (cast (typeless-cell #f) <boolean>))

  (unless typeless-bool
    (ann typeless-bool #f))))

(define-test "(if) propagates test truthiness information through variable assignment" (expect-success
  (import (llambda typed))

  (define typeless-bool (cast (typeless-cell #f) <boolean>))
  (define typeless-alias typeless-bool)

  (if typeless-bool
    (ann typeless-alias #t)
    (ann typeless-alias #f))))

(define-test "branching on type predicates propagates type information" (expect-success
  (import (llambda typed))

  (define string-or-symbol (cast (typeless-cell 'test) (U <string> <symbol>)))

  (if (string? string-or-symbol)
    (ann string-or-symbol <string>)
    (ann string-or-symbol <symbol>))))

(define-test "branching on type predicates with (not)" (expect-success
  (import (llambda typed))

  (define string-or-symbol (cast (typeless-cell 'test) (U <string> <symbol>)))

  (if (not (string? string-or-symbol))
    (ann string-or-symbol <symbol>)
    (ann string-or-symbol <string>))))

(define-test "branching on type predicates with (and)" (expect-success
  (import (llambda typed))

  (define typeless-string (cast (typeless-cell "test") <any>))
  (define typeless-symbol (cast (typeless-cell 'test) <any>))

  (if (and (string? typeless-string) (symbol? typeless-symbol))
    (begin
      (ann typeless-string <string>)
      (ann typeless-symbol <symbol>)))))

(define-test "branching on type predicates with (cond)" (expect-success
  (import (llambda typed))

  (define typeless-string (cast (typeless-cell "test") (U <string> <integer>)))
  (define typeless-symbol (cast (typeless-cell 'test) <any>))

  (cond
    ((string? typeless-string)
     (ann typeless-string <string>))
    ((symbol? typeless-symbol)
     (ann typeless-symbol <symbol>))
    (else
     (ann typeless-string <integer>)))))

(define-test "branching on type predicates with (or)" (expect-success
  (import (llambda typed))

  (define typeless-string (cast (typeless-cell "test") <any>))

  ; This is much looser than the other occurrence tests as we aren't looking for a specific type. Theoretically we could
  ; derive this type as (U <symbol> <string>) but the current system doesn't and its dobutful if it's worth adding the
  ; complexity. Instead just ensure that we derive a type that includes both <string> and <symbol> to make sure the type
  ; constraint system isn't over-constraining the type.
  (if (or (string? typeless-string) (symbol? typeless-string))
    (when (dynamic-false)
      ; Note that this is (cast), not (ann). This means the type conversion only needs to be possible, not proven.
      (cast typeless-string <string>)
      (cast typeless-string <symbol>)))))

(define-test "branching on (eqv?) propagates type information" (expect-success
  (import (llambda typed))

  (define typeless-null (typeless-cell '()))

  (when (eqv? '() typeless-null)
    (ann typeless-null <empty-list>))))

(define-test "branching on (equal?) propagates type information" (expect-success
  (import (llambda typed))

  (define typeless-bool (cast (typeless-cell #f) <boolean>))

  (if (equal? #t typeless-bool)
    (ann typeless-bool #t)
    (ann typeless-bool #f))))

(define-test "branch on (if) result with false in one branch" (expect-success
  (import (llambda typed))

  (define typeless-bool (cast (typeless-cell #f) <boolean>))
  (define typeless-symbol (cast (typeless-cell 'symbol) <symbol>))

  (define cond-result (if typeless-bool
    typeless-symbol
    #f))

  (if cond-result
    (ann cond-result <symbol>))))

(define-test "(if) with terminating branch uses other branch type information afterwards" (expect-success
  (import (llambda typed))

  (define string-or-int (typed-dynamic 5 (U <string> <integer>)))

  (if (string? string-or-int)
    (error "I hate strings!"))

  (ann string-or-int <integer>)))
