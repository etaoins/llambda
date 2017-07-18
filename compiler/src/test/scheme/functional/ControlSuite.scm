(define-test "(vector-map)" (expect-success
  (assert-equal #(b e h) (vector-map cadr '#((a b) (d e) (g h))))
  (assert-equal #(1 4 27 256 3125) (vector-map (lambda (n) (expt n n)) '#(1 2 3 4 5)))
  (assert-equal #(5 7 9) (vector-map + '#(1 2 3) '#(4 5 6 7)))
  (assert-equal #(1 2 3) (vector-map (lambda (x) x) '#(1 2 3)))

  (assert-equal #(4 5) (vector-map (lambda (x y) y) #(1 2 3) #(4 5)))

  (assert-equal #(1 2)
                (let ((count 0))
                  (vector-map
                    (lambda (ignored)
                      (set! count (+ count 1))
                      count)
                    '#(a b))))))

(define-test "(vector-for-each)" (expect-success
  (import (llambda typed))

  (assert-equal #(0 1 4 9 16)
                (let ((v (make-vector 5)))
                  (vector-for-each
                    (lambda (i) (vector-set! v i (* i i)))
                    '#(0 1 2 3 4))
                  v))

  (define counter : <integer> 0)

  (vector-for-each
    (lambda (i)
      (set! counter (+ counter 1)))
    #(a b c d e f))

  (assert-equal 6 counter)))

(define-test "(map)" (expect-success
  (assert-equal '(b e h) (map cadr '((a b) (d e) (g h))))
  (assert-equal '(1 4 27 256 3125) (map (lambda (n) (expt n n)) '(1 2 3 4 5)))
  (assert-equal '(5 7 9) (map + '(1 2 3) '(4 5 6 7)))
  (assert-equal '(1 2 3) (map (lambda (x) x) '(1 2 3)))

  (assert-equal '(1 2)
                (let ((count 0))
                  (map
                    (lambda (ignored)
                      (set! count (+ count 1))
                      count)
                    '(a b))))))

(define-test "(for-each)" (expect-success
  (import (llambda typed))

  (assert-equal #(0 1 4 9 16)
                (let ((v (make-vector 5)))
                  (for-each
                    (lambda (i) (vector-set! v i (* i i)))
                    '(0 1 2 3 4))
                  v))

  (define acc : <integer> 0)

  (for-each
    (lambda (i)
      (set! acc (+ acc i)))
    '(1 2 3 4 5))

  (assert-equal 15 acc)))

(define-test "(string-map)" (expect-success
  (import (llambda char))
  (assert-equal "abdegh" (string-map char-foldcase "AbdEgH"))

  (assert-equal "IBM" (string-map
                        (lambda (c)
                          (integer->char (+ 1 (char->integer c))))
                        "HAL"))

  (assert-equal "StUdLyCaPs" (string-map
                               (lambda (c k)
                                 ((if (eqv? k #\u) char-upcase char-downcase)
                                  c))
                               "studlycaps xxx"
                               "ululululul"))))

(define-test "(string-for-each)" (expect (101 100 99 98 97)
  (let ((v '()))
    (string-for-each
      (lambda (c) (set! v (cons (char->integer c) v)))
      "abcde")
    v)))

(define-test "(fold)" (expect-success
  (import (llambda list))
  (import (llambda typed))

  (assert-equal 10 (ann (fold + 0 '(1 2 3 4)) <number>))
  (assert-equal '(four three two one) (ann (fold cons '() '(one two three four)) <list-element>))
  (assert-equal '() (ann (fold cons '() '()) <list-element>))

  (define symbol-count (fold (lambda (x [count : <integer>]) (if (symbol? x) (+ count 1) count))
                             0
                             '(one 2 three four 5 six)))

  (assert-equal 4 (ann symbol-count <integer>))

  (assert-equal '(c 3 b 2 a 1) (fold cons* '() '(a b c) '(1 2 3 4 5)))))

(define-test "(fold) with improper list fails" (expect-error type-error?
  (import (llambda list))

  (fold + 0 '(1 2 3 . 4))))

(define-test "(reduce)" (expect-success
  (import (llambda list))
  (import (llambda typed))

  (assert-equal #f (ann (reduce max #f '()) (U #f <number>)))
  (assert-equal 44 (ann (reduce max #f '(44)) (U #f <number>)))
  (assert-equal 192 (ann (reduce max #f '(44 -123 57 192)) (U #f <number>)))
  (assert-equal -123 (ann (reduce min #f '(44 -123 57 192)) (U #f <number>)))))

(define-test "(reduce) with improper list fails" (expect-error type-error?
  (import (llambda list))

  (reduce max #f '(44 -123 57 . 192))))

(define-test "(append-map)" (expect-success
  (import (llambda list))

  (assert-equal '(1 -1 3 -3 8 -8) (append-map (lambda (x) (list x (- x))) '(1 3 8)))
  (assert-equal '() (append-map (lambda (x) (list x (- x))) '()))
  (assert-equal '(1 4 7 2 5 8 3 6 9) (append-map (lambda args args) '(1 2 3) '(4 5 6) '(7 8 9 10)))))

(define-test "(append-map) with improper list fails" (expect-error type-error?
  (import (llambda list))
  (append-map (lambda (x) (list x (- x))) '(1 3 . 8))))

(define-test "(filter-map)" (expect-success
  (import (llambda list))
  (import (llambda typed))

  ; XXX: #f can't really appear but our type system isn't rich enough to subtract types at the moment so we include all
  ; possible returns from the lambda
  (assert-equal '(1 9 49) (ann (filter-map (lambda (x) (and (number? x) (* x x))) '(a 1 b 3 c 7)) (Listof (U <number> #f))))
  (assert-equal '() (filter-map (lambda (x) (and (number? x) (* x x))) '()))
  (assert-equal '((1 one) (3 Three three)) (filter-map (lambda (first . rest) (member first rest)) '(1 2 3 4) '(one two 3) '(1 two Three) '(one TWO three)))))

(define-test "(filter-map) on improper list fails" (expect-error type-error?
  (import (llambda list))
  (filter-map (lambda (x) (and (number? x) (* x x))) '(a 1 b 3 c . 7))))
