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

(define-test "(hash-map-ref)" (expect-success
  (import (llambda hash-map))
  (import (llambda typed))
  (import (llambda error))

  (define number-hash-map (alist->hash-map '((1 . one) (2 . one) (3 . three) (2 . two))))
  (ann number-hash-map (HashMap <exact-integer> <symbol>))

  (assert-equal 3 (hash-map-size number-hash-map))
  (assert-equal 'one (hash-map-ref number-hash-map 1))
  (assert-equal 'two (hash-map-ref number-hash-map 2))
  (assert-equal 'three (hash-map-ref number-hash-map 3))
  (assert-equal 'four (hash-map-ref number-hash-map 4 (lambda () 'four)))

  (assert-raises invalid-argument-error?
    (assert-equal #f (hash-map-ref number-hash-map 4)))))

(define-test "(hash-map->alist)" (expect-success
  (import (llambda hash-map))
  (import (llambda typed))

  (define number-hash-map (alist->hash-map '((1 . one) (2 . one) (3 . three) (2 . two))))

  (define number-assoc (hash-map->alist number-hash-map))
  (ann number-assoc (Listof (Pairof <exact-integer> <symbol>)))

  (assert-equal 3 (length number-assoc))

  (assert-equal 'one (cdr (assoc 1 number-assoc)))
  (assert-equal 'two (cdr (assoc 2 number-assoc)))
  (assert-equal 'three (cdr (assoc 3 number-assoc)))
  (assert-equal #f (assoc 4 number-assoc))))

(define-test "(hash-map-keys)" (expect-success
  (import (llambda hash-map))
  (import (llambda typed))

  (define number-hash-map (alist->hash-map '((1 . one) (2 . one) (3 . three) (2 . two))))

  (define number-keys (hash-map-keys number-hash-map))
  (ann number-keys (Listof <exact-integer>))

  (assert-equal 3 (length number-keys))

  (assert-false (not (member 1 number-keys)))
  (assert-false (not (member 2 number-keys)))
  (assert-false (not (member 3 number-keys)))
  (assert-false (member 4 number-keys))))

(define-test "(hash-map-values)" (expect-success
  (import (llambda hash-map))
  (import (llambda typed))

  (define number-hash-map (alist->hash-map '((1 . one) (2 . one) (3 . three) (2 . two))))

  (define number-values (hash-map-values number-hash-map))
  (ann number-values (Listof <symbol>))

  (assert-equal 3 (length number-values))

  (assert-false (not (member 'one number-values)))
  (assert-false (not (member 'two number-values)))
  (assert-false (not (member 'three number-values)))
  (assert-false (member 'four number-values))))

(define-test "(hash-map-for-each)" (expect-success
  (import (llambda hash-map))
  (import (llambda typed))

  (define number-hash-map (alist->hash-map '((1 . one) (2 . one) (3 . three) (2 . two))))

  (define seen-1 #f)
  (define seen-2 #f)
  (define seen-3 #f)

  (hash-map-for-each (lambda (key value)
                       (case key
                         ((1)
                          (assert-false seen-1)
                          (set! seen-1 #t))
                         ((2)
                          (assert-false seen-2)
                          (set! seen-2 #t))
                         ((3)
                          (assert-false seen-3)
                          (set! seen-3 #t))
                         (else (error "Unexpected key" key)))) number-hash-map)

  (assert-true seen-1)
  (assert-true seen-2)
  (assert-true seen-3)))

(define-test "(hash-map-fold)" (expect-success
  (import (llambda hash-map))
  (import (llambda typed))

  (define number-hash-map (alist->hash-map '((1 . "one") (2 . "one") (3 . "three") (2 . "two"))))

  (define total (hash-map-fold (lambda (key value accum)
                                 {accum + key + {(string-length value) * 10}}
                                 ) 0 number-hash-map))

  (assert-equal 116 total)))

(define-test "(hash)" (expect-success
  (import (llambda hash-map))
  (import (llambda typed))

  (assert-equal (hash "One") (hash "One"))
  (assert-equal (hash 'One) (hash 'One))
  (assert-equal (hash 1) (hash 1))
  (assert-equal (hash #(1)) (hash #(1)))
  (assert-equal (hash '(1)) (hash '(1)))

  (assert-true (< (hash "Japan" 1) 1))
  (assert-true (< (hash "France" 2) 2))
  (assert-true (< (hash "United Kingdom" 3) 3))
  (assert-true (< (hash "Germany" 4) 4))))
