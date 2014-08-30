(define-test "(pair?)" (expect-success
	(assert-true  (pair? '(a . b)))
	(assert-true  (pair? (typeless-cell '(a . b))))

	(assert-true  (pair? '(a  b c)))
	(assert-true  (pair? (typeless-cell '(a  b c))))

  (assert-false (pair? '()))
  (assert-false (pair? (typeless-cell '())))

  (assert-false (pair? #(a b)))
  (assert-false (pair? (typeless-cell '())))))

(define-test "(null?)" (expect-success
	(assert-true  (null? '()))
	(assert-true  (null? (typeless-cell '())))

  (assert-false (null? '(a b c)))
  (assert-false (null? (typeless-cell '(a b c))))

  (assert-false (null? #(a b c)))
  (assert-false (null? (typeless-cell #(a b c)))))) 

(define-test "(list?)" (expect-success
   (assert-true  (list? '(a b c)))
   (assert-true  (list? '()))
   (assert-false (list? '(a . b)))))

(define-test "(cons)" (expect-success
	(assert-equal '(a)
                (cons 'a '()))

  (assert-equal '((a) b c d)
                (cons '(a) '(b c d)))

  (assert-equal '("a" b c)
                (cons "a" '(b c)))

  (assert-equal '(a . 3)
                (cons 'a 3))

  (assert-equal '((a b) . c)
                (cons '(a b) 'c))))


(define-test "(car)" (expect-success
  (assert-equal 'a (car '(a b c)))
  (assert-equal '(a) (car '((a) b c)))
	(assert-equal 1 (car '(1 . 2)))))

(define-test "(length)" (expect-success
  (assert-equal 3 (length '(a b c)))
	(assert-equal 3 (length '(a (b) (c d e))))
	(assert-equal 0 (length '()))))

(define-test "length of improper list fails" (expect-failure
	(length '(1 . 2))))

(define-test "make-list" (expect-success 
	(assert-equal '() (make-list 0))
  (assert-equal '() (make-list 0 4.0))
  (assert-equal '(#!unit #!unit #!unit #!unit) (make-list 4))
  (assert-equal '(4.0 4.0 4.0 4.0) (make-list 4 4.0))))

(define-test "(list-copy) of degenerate lists" (expect-success
  (assert-equal '() (list-copy '()))
	(assert-equal '(1 2 . 3) (list-copy '(1 2 . 3)))
  ; This is allowrequired by R7RS
  ; Single objects can also be considered degenerate forms of improper lists so
  ; this makes some sense
  (assert-equal 'a (list-copy 'a))))


(cond-expand
  (immutable-pairs
    (define-test "(list-copy) of non-empty proper list" (expect-success
      (define immutable-list '(1.0 2.0 3.0))
      (define copied-list (list-copy immutable-list))

      (assert-equal '(1.0 2.0 3.0) copied-list))))
  (else
    (define-test "(list-copy) of non-empty proper list" (expect-success
      (define immutable-list '(1.0 2.0 3.0))
      (define copied-list (list-copy immutable-list))
      ; This shouldn't effect the immutable list
      (set-car! copied-list -1.0)

      (assert-equal '(1.0 2.0 3.0) immutable-list)
      (assert-equal '(-1.0 2.0 3.0) copied-list)))))

(define-test "(list)" (expect-success
	(assert-equal '() (list))
	(assert-equal '(1 2 3) (list 1 2 3))))

(define-test "(append)" (expect-success
	(assert-equal '() (append))
	(assert-equal 'a (append 'a))
	(assert-equal '(1 2 3 4 5 6) (append '(1 2) '(3 4) '(5 6)))
  (assert-equal '() (append '() '() '()))
  (assert-equal 'a (append '() 'a))))

(define-test "(append) with non-terminal non-list fails" (expect-failure
	(append '(1 2) 3 '(4 5))))

(define-test "(memq)" (expect-success
	(assert-equal '(a b c) (memq 'a '(a b c)))
  (assert-equal '(b c) (memq 'b '(a b c)))
  (assert-false (memq 'a '(b c d)))

  (cond-expand ((not immutable-pairs)
    ; memq isn't recurive
    (assert-false (memq (list 'a) '(b (a) c)))))))

(define-test "(member) is recursive" (expect ((a) c)
	(member (list 'a) '(b (a) c))))

; This is technically unspecified for memq because integer comparison is
; unspecified for eq?
(define-test "(memv) on number list" (expect (101 102)
	(memv 101 '(100 101 102))))

(cond-expand ((not immutable-pairs)
  (define-test "(set-car!) of cons" (expect (new-car . old-cdr)
    (define test-cons (cons 'old-car 'old-cdr))
    (set-car! test-cons 'new-car)
    test-cons))

  (define-test "(set-car!) on literal fails" (expect-failure
    (set-car! '(old-car . old-cdr) 'new-car)))

  (define-test "(set-cdr!) of cons" (expect (old-car . new-cdr)
    (define test-cons (cons 'old-car 'old-cdr))
    (set-cdr! test-cons 'new-cdr)
    test-cons))

  (define-test "(set-cdr!) on literal fails" (expect-failure
    (set-cdr! '(old-car . old-cdr) 'new-cdr)))))

(define-test "(reverse)" (expect-success
  (assert-equal '() (reverse '()))
  (assert-equal '(c b a) (reverse '(a b c)))
  (assert-equal '((e (f)) d (b c) a) (reverse '(a (b c) d (e (f)))))))

(define-test "base cadr procedures" (expect-success
  (define test-list '((1 . 2) (3 . 4)))

  (assert-equal 1 (caar test-list))
  (assert-equal '(3 . 4) (cadr test-list))
  (assert-equal 2 (cdar test-list))
  (assert-equal '() (cddr test-list))))

(define-test "association lists" (expect-success
  (define e '((a 1)(b 2)(c 3)))

  (assert-equal '(a 1) (assq 'a e))
  (assert-equal '(b 2) (assq 'b e))
  (assert-equal #f (assq 'd e))

  (cond-expand ((not immutable-pairs)
    (assert-equal #f (assq (list 'a) '(((a)) ((b)) ((c)))))))

  (assert-equal '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))

  (assert-equal '(5 7) (assv 5 '((2 3) (5 7) (11 13))))))
