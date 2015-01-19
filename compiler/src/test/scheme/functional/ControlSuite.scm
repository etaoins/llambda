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

(define-test "trivial (call/cc) invoking escape procedure" (expect 5
	(call/cc (lambda (return)
		(return 5)))))

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

(define-test "nested (call/cc) invoking both escape procedures" (expect 15
	(call/cc (lambda (outer-return)
		(outer-return (call/cc (lambda (inner-return)
			(inner-return 15)
		)))
	))))

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
		(if dynamic-true 
		  (return 'one)
		  (return 'two))
	))))

(define-test "(call/cc) exiting from one branch side" (expect one
	(call/cc (lambda (return)
		(if dynamic-true 
		  (return 'one)
		  #f)
	))))

(define-test "(define) doesn't accept multiple values" (expect-error arity-error?
  (define x (values 1 2 3))))

(define-test "(define) accepts single value produced with (values)" (expect test
  (define x (values 'test))
  x))

(define-test "(call-with-values) with single value" (expect -1
  (call-with-values * -)))

(define-test "(call-with-values) with wrong producer arity fails at compile time" (expect-compile-error arity-error?
  (call-with-values (lambda (extra-arg)) -)))

(define-test "(call-with-values) with multiple values" (expect 5
  (call-with-values (lambda () (values 4 5))
                    (lambda (a b) b))))

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
    (if dynamic-true
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
