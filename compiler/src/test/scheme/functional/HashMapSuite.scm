(define-test "(make-hash-map)" (expect-success
  (import (llambda hash-map))

  (define empty-hash-map (make-hash-map))

  (assert-true  (hash-map? empty-hash-map))
  (assert-false (hash-map? 1))
  (assert-false (hash-map? #f))

  (assert-equal 0 (hash-map-size empty-hash-map))))

(define-test "(hash-map-assoc)" (expect-success
  (import (llambda hash-map))
  (import (llambda typed))

  (define empty-hash-map (make-hash-map))

  (define one-value-hash-map (hash-map-assoc empty-hash-map 1 'one))

  (ann one-value-hash-map (HashMap <exact-integer> 'one))

  (assert-true  (hash-map-exists? one-value-hash-map 1))
  (assert-equal 'one (ann (hash-map-ref/default one-value-hash-map 1 #f) (U #f 'one)))
  (assert-false (hash-map-exists? one-value-hash-map 2.0))
  (assert-equal #f (hash-map-ref/default one-value-hash-map 2.0 #f))
  (assert-false (hash-map-exists? one-value-hash-map 'three))
  (assert-equal #f (hash-map-ref/default one-value-hash-map 'three #f))

  (assert-equal 1 (hash-map-size one-value-hash-map))

  (define two-value-hash-map (hash-map-assoc one-value-hash-map 2.0 'two))

  (ann two-value-hash-map (HashMap <number> <symbol>))
  (assert-equal 2 (hash-map-size two-value-hash-map))

  (assert-true  (hash-map-exists? two-value-hash-map 1))
  (assert-equal 'one (ann (hash-map-ref/default two-value-hash-map 1 #f) (U #f 'one 'two)))
  (assert-true  (hash-map-exists? two-value-hash-map 2.0))
  (assert-equal 'two (hash-map-ref/default two-value-hash-map 2.0 #f))
  (assert-false (hash-map-exists? two-value-hash-map 'three))
  (assert-equal #f (hash-map-ref/default two-value-hash-map 'three #f))))

(define-test "(hash-map-delete)" (expect-success
  (import (llambda hash-map))

  (define hash-map (make-hash-map))
  (set! hash-map (hash-map-assoc hash-map 'one 1))
  (set! hash-map (hash-map-assoc hash-map 'two 2))
  (set! hash-map (hash-map-assoc hash-map 'three 3))

  (define hash-map-wo-one (hash-map-delete hash-map 'one))
  (assert-equal 2 (hash-map-size hash-map-wo-one))
  (assert-false (hash-map-exists? hash-map-wo-one 'one))
  (assert-true  (hash-map-exists? hash-map-wo-one 'two))
  (assert-true  (hash-map-exists? hash-map-wo-one 'three))

  (define hash-map-wo-two (hash-map-delete hash-map-wo-one 'two))
  (assert-equal 1 (hash-map-size hash-map-wo-two))
  (assert-false (hash-map-exists? hash-map-wo-two 'one))
  (assert-false (hash-map-exists? hash-map-wo-two 'two))
  (assert-true  (hash-map-exists? hash-map-wo-two 'three))

  (define hash-map-wo-three (hash-map-delete hash-map-wo-two 'three))
  (assert-equal 0 (hash-map-size hash-map-wo-three))
  (assert-false (hash-map-exists? hash-map-wo-three 'one))
  (assert-false (hash-map-exists? hash-map-wo-three 'two))
  (assert-false (hash-map-exists? hash-map-wo-three 'three))))

(define-test "(alist->hash-map)" (expect-success
  (import (llambda hash-map))
  (import (llambda typed))

  (define empty-hash-map (alist->hash-map '()))
  (assert-equal 0 (hash-map-size empty-hash-map))

  (define number-hash-map (alist->hash-map '((1 . one) (2 . one) (3 . three) (2 . two))))
  (ann number-hash-map (HashMap <exact-integer> <symbol>))

  (assert-equal 3 (hash-map-size number-hash-map))
  (assert-equal 'one (hash-map-ref/default number-hash-map 1 #f))
  (assert-equal 'two (hash-map-ref/default number-hash-map 2 #f))
  (assert-equal 'three (hash-map-ref/default number-hash-map 3 #f))
  (assert-equal #f (hash-map-ref/default number-hash-map 4 #f))))
