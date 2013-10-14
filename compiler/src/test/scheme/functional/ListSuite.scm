(define-test "improper is pair" (expect #t
	(import (scheme core))
	(pair? '(a . b))))

(define-test "proper is pair" (expect #t
	(import (scheme core))
	(pair? '(a  b c))))

(define-test "empty list isn't pair" (expect #f
	(import (scheme core))
	(pair? '())))

(define-test "empty list is null" (expect #t
	(import (scheme core))
	(null? '())))

(define-test "proper isn't null" (expect #f
	(import (scheme core))
	(null? '(a  b c))))

(define-test "vector isn't pair" (expect #f
	(import (scheme core))
	(pair? '#(a b))))

(define-test "cons simple proper list" (expect (a)
	(import (scheme core))
	(cons 'a '())))

(define-test "cons nested proper list" (expect ((a) b c d)
	(import (scheme core))
	(cons '(a) '(b c d))))

(define-test "cons proper list with string head" (expect ("a" b c)
	(import (scheme core))
	(cons "a" '(b c))))

(define-test "cons simple improper list" (expect (a . 3)
	(import (scheme core))
	(cons 'a 3)))

(define-test "cons simple improper list with nested proper list" (expect ((a b) . c)
	(import (scheme core))
	(cons '(a b) 'c)))

(define-test "car of proper list" (expect a
	(import (scheme core))
	(car '(a b c))))

(define-test "car of nested list" (expect (a)
	(import (scheme core))
	(car '((a) b c d))))

(define-test "car of improper list" (expect 1
	(import (scheme core))
	(car '(1 . 2))))

(define-test "length of proper list" (expect 3
	(import (scheme core))
	(length '(a b c))))

(define-test "length of nested list" (expect 3
	(import (scheme core))
	(length '(a (b) (c d e)))))

(define-test "length of empty list" (expect 0
	(import (scheme core))
	(length '())))
