(define-test "trivial immediate (force)" (expect 3
  (import (scheme lazy))

  (define p (delay (+ 1 2)))
  (assert-true (promise? p))
  (force p)))

(define-test "trivial (make-promise)" (expect 3
  (import (scheme lazy))

  (define p (make-promise 3))
  (assert-true (promise? p))
  (force p)))

(define-test "trivial (delay-force)" (expect 3
  (import (scheme lazy))

  (define p (delay-force (delay (+ 1 2))))
  (assert-true (promise? p))
  (force p)))

(define-test "(force)ing twice produces the same value" (expect (3 3)
  (import (scheme lazy))

  (let ((p (delay (+ 1 2))))
    (list (force p) (force p)))))

(define-test "recursive promises and streaming" (expect-success
  (import (scheme lazy))

  (define integers
    (letrec ((next
               (lambda (n)
                 (delay (cons n (next (+ n 1)))))))
      (next 0)))
  (define head
    (lambda (stream) (car (force stream))))
  (define tail
    (lambda (stream) (cdr (force stream))))

  (assert-equal 2 (head (tail (tail integers))))

  (define (stream-filter p? s)
    (delay-force
      (if (null? (force s))
        (delay '())
        (let ((h (car (force s)))
              (t (cdr (force s))))
          (if (p? h)
            (delay (cons h (stream-filter p? t)))
            (stream-filter p? t))))))
  (assert-equal 5 (head (tail (tail (stream-filter odd? integers)))))))

(define-test "(force) only computes a value once" (expect-success
  (import (scheme lazy))
  (define count 0)
  (define x 5)
  (define p
    (delay (begin (set! count (+ count 1))
                  (if (> count x)
                    count
                    (force p)))))
  (assert-true (promise? p))
  (assert-equal 6 (force p))
  (assert-true (promise? p))
  (assert-equal 6
    (begin (set! x 10)
           (force p)))))

(define-test "(llambda lazy) provides <promise> type" (expect-success
  (import (llambda lazy))
  (import (llambda typed))

  (define p (delay (+ 1 2)))
  (assert-true ((make-predicate <promise>) p))))

(define-test "SRFI-45 memoization test 1" (expect-output (hello)
  (import (scheme lazy))
  (import (scheme write))

  (define s (delay (begin (write 'hello) 1)))

  (force s)
  (force s)))

(define-test "SRFI-45 memoization test 2" (expect-output (bonjour)
  (import (scheme lazy))
  (import (scheme write))

  (let ((s (delay (begin (display 'bonjour) 2))))
    (+ (force s) (force s)))))

(define-test "SRFI-45 memoization test 3" (expect-output (hi)
  (import (scheme lazy))
  (import (scheme write))

  (define r (delay (begin (display 'hi) 1)))
  (define s (delay-force r))
  (define t (delay-force s))

  (force t)
  (force r)))

; SRFI-45 reentry test 1 is essentially the same as "(force) only computes a value once"

(define-test "SRFI-45 reentrancy test 2" (expect-success
  (import (scheme lazy))
  (import (scheme write))

  (define f
    (let ((first? #t))
      (delay
        (if first?
          (begin
            (set! first? #f)
            (force f))
          'second))))

  (assert-equal 'second (force f))))

(define-test "SRFI-45 reentrancy test 3" (expect-success
  (import (scheme lazy))
  (import (scheme write))

  (define q
    (let ((count 5))
      (define (get-count) count)
      (define p (delay (if (<= count 0)
                         count
                         (begin (set! count (- count 1))
                                (force p)
                                (set! count (+ count 2))
                                count))))
      (list get-count p)))
  (define get-count (car q))
  (define p (cadr q))

  (assert-equal 5 (get-count))
  (assert-equal 0 (force p))
  (assert-equal 10 (get-count))))

(define-test "SRFI-45 example procedures" (expect-success
  (import (scheme lazy))

  (define-syntax match
    (syntax-rules ()
                  ((match exp
                          (()      exp1)
                          ((h . t) exp2))
                   (let ((lst exp))
                     (cond ((null? lst) exp1)
                           ((pair? lst) (let ((h (car lst))
                                              (t (cdr lst)))
                                          exp2))
                           (else 'match-error))))))

  (define (from n)
      (delay (cons n (from (+ n 1)))))

  (define (stream-filter p? s)
    (delay-force (match (force s)
                 (()      (delay '()))
                 ((h . t) (if (p? h)
                            (delay (cons h (stream-filter p? t)))
                            (stream-filter p? t))))))

  (define (stream-ref s index)
    (delay-force
      (match (force s)
             (()      'error)
             ((h . t) (if (zero? index)
                        (delay h)
                        (stream-ref t (- index 1)))))))

  ; Check that evenness is correctly implemented - should terminate:
  (assert-equal 0 (force (stream-ref (stream-filter zero? (from 0)) 0)))

  (define s (stream-ref (from 0) 6))
  (assert-true (promise? s))
  (assert-equal 6 (force s))))
