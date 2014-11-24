(define-test "(do) with two stepping variables" (expect 25
    (let ((x '(1 3 5 7 9)))
      (do ((x x (cdr x))
           (sum 0 (+ sum (car x))))
        ((null? x) sum)))))

(define-test "(do) with one stepping, one constant variable" (expect #(0 1 2 3 4)
    (do ((vec (make-vector 5 #!unit))
         (i 0 (+ i 1)))
      ((= i 5) vec)
      (vector-set! vec i i))))

(define-test "named let" (expect ((6 1 3) (-5 -2))
  (let loop ((numbers '(3 -2 1 6 -5))
             (nonneg '())
             (neg '()))
    (cond ((null? numbers) (list nonneg neg))
          ((>= (car numbers) 0)
           (loop (cdr numbers)
                 (cons (car numbers) nonneg)
                 neg))
          ((< (car numbers) 0)
           (loop (cdr numbers)
                 nonneg
                 (cons (car numbers) neg)))))))

(define-test "typed named let" (expect ((6 1 3) (-5 -2))
  (import (llambda typed))
  (let loop ([numbers : (Listof <exact-integer>) '(3 -2 1 6 -5)]
             [nonneg : (Listof <exact-integer>) '()]
             [neg : (Listof <exact-integer>) '()])
    (cond ((null? numbers) (list nonneg neg))
          ((>= (car numbers) 0)
           (loop (cdr numbers)
                 (cons (car numbers) nonneg)
                 neg))
          ((< (car numbers) 0)
           (loop (cdr numbers)
                 nonneg
                 (cons (car numbers) neg)))))))
