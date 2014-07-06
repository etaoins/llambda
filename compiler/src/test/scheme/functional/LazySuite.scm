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
