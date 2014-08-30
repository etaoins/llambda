(define-test "applying fixed arg storage locs constrains their type" (expect-success
  (import (llambda typed))
  (define typeless-vector (typeless-cell #(1 2 3)))
  (define typeless-1 (typeless-cell 1))

  (define ref-result (vector-ref typeless-vector typeless-1))

  (assert-equal #(1 2 3) (ann typeless-vector <vector>))
  (assert-equal 1 (ann typeless-1 <exact-integer>))
  (assert-equal 2 ref-result)))

(define-test "applying rest arg storage locs constrains their type" (expect-success
  (import (llambda typed))
  (define typeless-2 (typeless-cell 2))
  (define typeless-3 (typeless-cell 3))

  (define *-result (* typeless-2 typeless-3))

  (assert-equal 2 (ann typeless-2 <number>))
  (assert-equal 3 (ann typeless-3 <number>))
  (assert-equal 6 *-result)))

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

  (define typeless-string (cast (typeless-cell "test") (U <string> <exact-integer>)))
  (define typeless-symbol (cast (typeless-cell 'test) <any>))

  (cond
    ((string? typeless-string)
     (ann typeless-string <string>))
    ((symbol? typeless-symbol)
     (ann typeless-symbol <symbol>))
    (else 
     (ann typeless-string <exact-integer>)))))

(define-test "branching on type predicates with (or)" (expect-success
  (import (llambda typed))

  (define typeless-string (cast (typeless-cell "test") <any>))

  ; This is much looser than the other occurrence tests as we aren't looking for a specific type. Theoretically we could
  ; derive this type as (U <symbol> <string>) but the current system doesn't and its dobutful if it's worth adding the
  ; complexity. Instead just ensure that we derive a type that includes both <string> and <symbol> to make sure the type
  ; constraint system isn't over-constraining the type.
  (if (or (string? typeless-string) (symbol? typeless-string))
    (when dynamic-false 
      ; Note that this is (cast), not (ann). This means the type conversion only needs to be possible, not proven.
      (cast typeless-string <string>)
      (cast typeless-string <symbol>)))))

(define-test "branch on (if) result with false in one branch" (expect-success
  (import (llambda typed))

  (define typeless-bool (cast (typeless-cell #f) <boolean>))
  (define typeless-symbol (cast (typeless-cell 'symbol) <symbol>))

  (define cond-result (if typeless-bool
    typeless-symbol
    #f))

  (if cond-result
    (ann cond-result <symbol>))))

(cond-expand ((not immutable-pairs)
  (define-test "(list?) occurrence typing doesn't survive a (set-cdr!)" (expect #f
    (import (llambda typed))
    (define test-list (list-copy '(1 2 3)))

    (if (list? test-list)
      (begin
        (set-cdr! test-list 2)
        (list? test-list))
      'unreachable)))))
