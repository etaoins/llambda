(define-test "(call/cc) escape procedure is a procedure" (expect #t
	(call/cc procedure?)))

(define-test "(call/cc) with non-procedure fails at compile time" (expect-compile-error type-error?
	(call/cc 4)))

(define-test "(call/cc) with procedure of incorrect arity fails at compile time" (expect-compile-error type-error?
	(call/cc cons)))

(define-test "captured (call/cc) escape procedure is a procedure" (expect #t
   (define captured-proc #f)
   (call/cc (lambda (escape-proc)
      (set! captured-proc escape-proc)))
   (procedure? captured-proc)))

(define-test "trivial (call/cc) invoking escape procedure" (expect-static-success
  (assert-equal 5
    (call/cc (lambda (return)
      (return 5))))))

(define-test "(call/cc) invoking escape procedure with multiple values" (expect 21
  (call-with-values
    (lambda ()
      (call/cc (lambda (return)
        (return * 7 3))))
    (lambda (proc . args)
      (apply proc args)))))

(define-test "trivial (call/cc) not invoking escape procedure" (expect 5
	(call/cc (lambda (return)
		5))))

(define-test "trivial (call/cc) only mutating escape procedure" (expect 5
	(call/cc (lambda (return)
    (set! return #f)
		5))))

(define-test "trivial (call/cc) mutating and invoking escape procedure" (expect -5
	(call/cc (lambda (return)
    (set! return -)
		(return 5)))))

(define-test "nested (call/cc) invoking both escape procedures" (expect-static-success
  (assert-equal 15
    (call/cc (lambda (outer-return)
      (outer-return (call/cc (lambda (inner-return)
        (inner-return 15)
      ))))))))

(define-test "nested (call/cc) invoking only inner escape procedure" (expect -15
	(call/cc (lambda (outer-return)
		(- (call/cc (lambda (inner-return)
			(inner-return 15)
		)))
	))))

(define-test "nested (call/cc) invoking only outer escape procedure" (expect 15
	(call/cc (lambda (outer-return)
		(call/cc (lambda (inner-return)
			(outer-return 15)
		))
        'shouldntreach
	))))

(define-test "(call/cc) exiting from both branch sides" (expect one
	(call/cc (lambda (return)
		(if (dynamic-true)
		  (return 'one)
		  (return 'two))
	))))

(define-test "(call/cc) exiting from one branch side" (expect one
	(call/cc (lambda (return)
		(if (dynamic-true)
		  (return 'one)
		  #f)
	))))

(define-test "(call/cc) R7RS examples" (expect-success
  (assert-equal -3
                (call-with-current-continuation
                  (lambda (exit)
                    (for-each (lambda (x)
                                (if (negative? x)
                                  (exit x)))
                              '(54 0 37 -3 245 19))
                    #t)))

  (define list-length
    (lambda (obj)
      (call-with-current-continuation
        (lambda (return)
          (letrec ((r
                     (lambda (obj)
                       (cond ((null? obj) 0)
                             ((pair? obj)
                              (+ (r (cdr obj)) 1))
                             (else (return #f))))))
            (r obj))))))

  (assert-equal 4 (list-length '(1 2 3 4)))
  (assert-equal #f (list-length '(a b . c)))))

(define-test "(define) doesn't accept multiple values" (expect-error arity-error?
  (define x (values 1 2 3))))

(define-test "(define) accepts single value produced with (values)" (expect test
  (define x (values 'test))
  x))

(define-test "(call-with-values) with single value" (expect -1
  (call-with-values * -)))

(define-test "(call-with-values) with wrong producer arity fails at compile time" (expect-compile-error arity-error?
  (call-with-values (lambda (extra-arg)) -)))

(define-test "(call-with-values) with multiple values" (expect-static-success
  (assert-equal 9
    (call-with-values (lambda () (values 4 5))
                      (lambda (a b) (+ a b))))))

(define-test "(call-with-values) with mismatched arity fails" (expect-error arity-error?
  (call-with-values (lambda () (values 4 5))
                    (lambda (a b c) b))))

(define-test "(call-with-values) with wrong type fails" (expect-error type-error?
  (import (llambda typed))
  (call-with-values (lambda () (values 4 5))
                    (lambda ([a : <exact-integer>] [b : <flonum>]) b))))

(define-test "(call-with-values) with zero values returning multiple values" (expect (a b c)
  (call-with-values
    (lambda ()
      (call-with-values (lambda () (values))
                        (lambda () (values 'a 'b 'c))))
    (lambda values-list
      values-list))))

(define-test "multiple values returned from (if)" (expect (1 2 3 4)
  (define (return-multiple)
    (if (dynamic-true)
      (values 1 2 3 4)
      (values 4 5 6 7)))

  (call-with-values return-multiple
                    (lambda values-list values-list))))

(define-test "captured continuation called multiple times" (expect (0 1 2 3 4 5 6 7 8 9 10)
  (define result-list '())
  (define captured-cont #!unit)

  (define callcc-result
    (call/cc 
      (lambda (cont)
        (set! captured-cont cont)
        (cont 0))))

  ; Append the result on to our result list so we can check it later
  (set! result-list (append result-list (list callcc-result)))

  ; Keep calling until the value is 10
  (if (< callcc-result 10)
    (captured-cont (+ callcc-result 1)))

  result-list))

(define-test "(vector-map)" (expect-success
  (assert-equal #(b e h) (vector-map cadr '#((a b) (d e) (g h))))
  (assert-equal #(1 4 27 256 3125) (vector-map (lambda (n) (expt n n)) '#(1 2 3 4 5)))
  (assert-equal #(5 7 9) (vector-map + '#(1 2 3) '#(4 5 6 7)))

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

  ; We should be able to iterate while returning mulitple values unlike (vector-map)
  (define counter : <exact-integer> 0)

  (vector-for-each
    (lambda (i)
      (set! counter (+ counter 1))
      (values i counter))
    #(a b c d e f))

  (assert-equal 6 counter)))

(define-test "(map)" (expect-success
  (assert-equal '(b e h) (map cadr '((a b) (d e) (g h))))
  (assert-equal '(1 4 27 256 3125) (map (lambda (n) (expt n n)) '(1 2 3 4 5)))
  (assert-equal '(5 7 9) (map + '(1 2 3) '(4 5 6 7)))

  (assert-equal '(1 2)
                (let ((count 0))
                  (map
                    (lambda (ignored)
                      (set! count (+ count 1))
                      count)
                    '(a b))))

  (cond-expand
    ((not immutable-pairs)
     (begin
       (define input-list (list-copy '(1 2 3 4)))

       (define (mapper-proc n)
         ; Mutate the list during (map) - this is an undefined operation but shouldn't crash
         (set-cdr! (cdr input-list) '())
         (* n n))


       (guard (condition
                (else 'ignore))
              (map mapper-proc input-list)))))))

(define-test "(for-each)" (expect-success
  (import (llambda typed))

  (assert-equal #(0 1 4 9 16)
                (let ((v (make-vector 5)))
                  (for-each
                    (lambda (i) (vector-set! v i (* i i)))
                    '(0 1 2 3 4))
                  v))

  ; We should be able to iterate while returning mulitple values unlike (vector-map)
  (define acc : <exact-integer> 0)

  (for-each
    (lambda (i)
      (set! acc (+ acc i))
      (values i acc))
    '(1 2 3 4 5))

  (assert-equal 15 acc)

  (cond-expand
    ((not immutable-pairs)
     (begin
       (define input-list (list-copy '(1 2 3 4)))

       (define (iter-proc n)
         ; Mutate the list during (for-each) - this is an undefined operation but shouldn't crash
         (set-cdr! (cdr input-list) '())
         (* n n))


       (guard (condition
                (else 'ignore))
              (for-each iter-proc input-list)))))))

(define-test "(string-map)" (expect-success
  (import (scheme char))
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

  (define symbol-count (fold (lambda (x [count : <exact-integer>]) (if (symbol? x) (+ count 1) count))
                             0
                             '(one 2 three four 5 six)))

  (assert-equal 4 (ann symbol-count <exact-integer>))

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
  (assert-equal '(1 4 7 2 5 8 3 6 9) (append-map (lambda args args) '(1 2 3) '(4 5 6) '(7 8 9 10)))

  (cond-expand
    ((not immutable-pairs)
     (begin
       (define input-list (list-copy '(1 2 3 4)))

       (define (map-proc n)
         (set-cdr! (cdr input-list) '())
         '())

       (guard (condition
                (else 'ignore))
              (append-map map-proc input-list)))))))

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
  (assert-equal '((1 one) (3 Three three)) (filter-map (lambda (first . rest) (member first rest)) '(1 2 3 4) '(one two 3) '(1 two Three) '(one TWO three)))

  (cond-expand
    ((not immutable-pairs)
     (begin
       (define input-list (list-copy '(1 2 3 4)))

       (define (map-proc n)
         (set-cdr! (cdr input-list) '())
         '())

       (guard (condition
                (else 'ignore))
              (filter-map map-proc input-list)))))))

(define-test "(filter-map) on improper list fails" (expect-error type-error?
  (import (llambda list))
  (filter-map (lambda (x) (and (number? x) (* x x))) '(a 1 b 3 c . 7))))
