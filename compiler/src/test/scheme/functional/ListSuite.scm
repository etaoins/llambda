(define-test "improper is pair" (expect #t
	(pair? '(a . b))))

(define-test "proper is pair" (expect #t
	(pair? '(a  b c))))

(define-test "empty list isn't pair" (expect #f
	(pair? '())))

(define-test "empty list is null" (expect #t
	(null? '())))

(define-test "proper isn't null" (expect #f
	(null? '(a  b c))))

(define-test "vector isn't pair" (expect #f
	(pair? '#(a b))))

(define-test "cons simple proper list" (expect (a)
	(cons 'a '())))

(define-test "cons nested proper list" (expect ((a) b c d)
	(cons '(a) '(b c d))))

(define-test "cons proper list with string head" (expect ("a" b c)
	(cons "a" '(b c))))

(define-test "cons simple improper list" (expect (a . 3)
	(cons 'a 3)))

(define-test "cons simple improper list with nested proper list" (expect ((a b) . c)
	(cons '(a b) 'c)))

(define-test "car of proper list" (expect a
	(car '(a b c))))

(define-test "car of nested list" (expect (a)
	(car '((a) b c d))))

(define-test "car of improper list" (expect 1
	(car '(1 . 2))))

(define-test "length of proper list" (expect 3
	(length '(a b c))))

(define-test "length of nested list" (expect 3
	(length '(a (b) (c d e)))))

(define-test "length of empty list" (expect 0
	(length '())))

(define-test "length of improper list fails" (expect-failure
	(length '(1 . 2))))

(define-test "make empty list" (expect ()
	(make-list 0 4.0)))

(define-test "make non-empty list" (expect (4.0 4.0 4.0 4.0)
	(make-list 4 4.0)))

(define-test "copy empty list" (expect ()
	(list-copy '())))

(define-test "copy non-empty list" (expect ((1.0 2.0 3.0) . (-1.0 2.0 3.0))
	(define immutable-list '(1.0 2.0 3.0))
	(define copied-list (list-copy immutable-list))
	; This shouldn't effect the immutable list
	(set-car! copied-list -1.0)
	(cons immutable-list copied-list)))

(define-test "copy improper list" (expect (1 2 . 3)
	(list-copy '(1 2 . 3))))

(define-test "empty (list)" (expect ()
	(list)))

(define-test "non-empty (list)" (expect (1 2 3)
	(list 1 2 3)))

(define-test "(append) with no arguments" (expect ()
	(append)))

(define-test "(append) with single argument returns it" (expect a
	(append 'a)))

(define-test "(append) three proper lists" (expect (1 2 3 4 5 6)
	(append '(1 2) '(3 4) '(5 6))))

(define-test "(append) three empty lists" (expect ()
	(append '() '() '())))

(define-test "(append) with non-terminal non-list fails" (expect-failure
	(append '(1 2) 3 '(4 5))))

(define-test "(append) empty list with symbol" (expect a
	(append '() 'a)))

