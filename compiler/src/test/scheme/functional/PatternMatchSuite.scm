(define-test "trivial matches" (expect-static-success
  (import (llambda match))

  (assert-equal 'foo
                (match "anything"
                       (_ 'foo)))

  (assert-equal "anything"
                (match "anything"
                       (same-val same-val)))))

(define-test "match failure" (expect-error match-error?
  (import (llambda match))

  (match 'other
         ('one 1)
         ('two 2)
         ('three 3))))

(define-test "equality matches" (expect-static-success
  (import (llambda match))

  (define (matcher val)
    (match val
           (1 'one)
           ("string" "str")
           (#f 'false)
           ('test-symbol 'symbol)
           (#u8(1 2 3) 'bv)
           ('(one two three) 'list-literal)
           (#(a b c) 'vector)))

  (assert-equal "str" (matcher "string"))
  (assert-equal 'one (matcher 1))
  (assert-equal 'false (matcher #f))
  (assert-equal 'symbol (matcher 'test-symbol))
  (assert-equal 'bv (matcher #u8(1 2 3)))
  (assert-equal 'list-literal (matcher '(one two three)))
  (assert-equal 'vector (matcher #(a b c)))))

(define-test "cons matches" (expect-static-success
  (import (llambda match))

  (define matcher
    (match-lambda
      ((cons 'left _) 'matches-left)
      ((cons _ 'right) 'matches-right)
      ((cons one (cons two (cons three '()))) (list three two one))
      ((cons captured-car captured-cdr) (list captured-car captured-cdr))))

  (assert-equal '(1 (2)) (matcher '(1 2)))
  (assert-equal '(3 2 1) (matcher '(1 2 3)))
  (assert-equal 'matches-left (matcher '(left . 2)))
  (assert-equal 'matches-right (matcher '(1 . right)))))

(define-test "vector matches" (expect-success
  (import (llambda match))

  (define (matcher val)
    (match val
           ((vector 1 two three) (list two three))
           ((vector one two three) one)))

  (assert-equal '(2 3) (matcher #(1 2 3)))
  (assert-equal 'first (matcher #(first second third)))))

(define-test "list matches" (expect-static-success
  (import (llambda match))

  (define matcher
    (match-lambda
      ((list) 'null)
      ((list _) 'one)
      ((list _ _) 'two)
      ((list first second third) second)))

  (assert-equal 'null (matcher '()))
  (assert-equal 'one (matcher '(1)))
  (assert-equal 'two (matcher '(1 2)))
  (assert-equal 2 (matcher '(1 2 3)))))

(define-test "type matches" (expect-static-success
  (import (llambda match))
  (import (llambda typed))

  (define matcher
    (match-lambda
      ((cons [int-var : <exact-integer>] _) (ann int-var <exact-integer>))
      ((cons _ [symbol-var : <symbol>]) (ann symbol-var <symbol>))
      (_ 'other)))

  (assert-equal 5 (matcher '(5 . symbol)))
  (assert-equal 'symbol (matcher '(#f . symbol)))
  (assert-equal 'other (matcher '(#\a . #\b)))))

(define-test "record type deconstruction" (expect-static-success
  (import (llambda match))
  (import (llambda typed))

  (define-record-type <two-fields> (two-fields first second) two-fields?
                      (first two-fields-first)
                      (second two-fields-second))

  (define-record-type <three-fields> (three-fields first second third) three-fields?
                      (first three-fields-first)
                      (second three-fields-second)
                      (third three-fields-third))

  (define matcher
    (match-lambda
      ((two-fields [first : <symbol>] second) 'starts-with-symbol)
      ((two-fields first second) second)
      ((three-fields first second third) second)))

  (assert-equal 2 (matcher (two-fields 1 2)))
  (assert-equal 'starts-with-symbol (matcher (two-fields 'one 2)))
  (assert-equal 'two (matcher (three-fields 'one 'two 'three)))))

(define-test "match has result type of a union of the clause results" (expect-success
  (import (llambda match))
  (import (llambda typed))

  (define match-result
    (match (typeless-cell 1)
           (1 'one)
           (2 'two)
           (3 'three)))

  (ann match-result (U 'one 'two 'three))))

(define-test "(match-lambda)" (expect-success
  (import (llambda match))

  (define sum-list (match-lambda
    ['() 0]
    [(cons head tail) (+ head (sum-list tail))]))

  (assert-equal 15 (sum-list '(1 2 3 4 5)))))
