; Taken from R7RS
(define-test "R7RS factorial" (expect 479001600
  #|
  The FACT procedure computes the factorial
  of a non-negative integer.
  |#
  (define fact
    (lambda (n)
      (if (= n 0)
        #;(= n 1)
        1 ;Base case: return 1
        (* n (fact (- n 1))))))

  (fact 12)))

; Taken from http://rosettacode.org/wiki/Sieve_of_Eratosthenes#Scheme
(define-test "simple sieve of Eratosthenes" (expect (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
  (import (llambda flonum))

  (define (iota start stop stride)
    (if (> start stop)
      (list)
      (cons start (iota (+ start stride) stop stride))))

  (define (strike lst start stride)
    (cond ((null? lst) lst)
      ((= (car lst) start) (strike (cdr lst) (+ start stride) stride))
      ((> (car lst) start) (strike lst (+ start stride) stride))
      (else (cons (car lst) (strike (cdr lst) start stride)))))

  (define (primes limit)
    (let ((stop (sqrt limit)))
      (define (sieve lst)
        (let ((p (car lst)))
          (if (> p stop)
            lst
            (cons p (sieve (strike (cdr lst) (* p p) p))))))
      (sieve (iota 2 limit 1))))

  (primes 100)))

(define-test "wheel-of-2 sieve of Eratosthenes" (expect (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
  (import (llambda flonum))

  (define (iota start stop stride)
    (if (> start stop)
      (list)
      (cons start (iota (+ start stride) stop stride))))

  (define (strike lst start stride)
    (cond ((null? lst) lst)
      ((= (car lst) start) (strike (cdr lst) (+ start stride) stride))
      ((> (car lst) start) (strike lst (+ start stride) stride))
      (else (cons (car lst) (strike (cdr lst) start stride)))))

  (define (primes-wheel-2 limit)
    (let ((stop (sqrt limit)))
      (define (sieve lst)
        (let ((p (car lst)))
          (if (> p stop)
            lst
            (cons p (sieve (strike (cdr lst) (* p p) (* 2 p)))))))
      (cons 2 (sieve (iota 3 limit 2)))))

  (primes-wheel-2 100)))

; Taken from http://rosettacode.org/wiki/Sieve_of_Eratosthenes#Scheme
(define-test "vector sieve of Eratosthenes" (expect (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
  (import (llambda flonum))

  ; initialize v to vector of sequential integers
  (define (initialize! v)
    (define (iter v n) (if (>= n (vector-length v))
                         #!unit
                         (begin (vector-set! v n n) (iter v (+ n 1)))))
    (iter v 0))

  ; set every nth element of vector v to 0,
  ; starting with element m
  (define (strike! v m n)
    (cond ((>= m (vector-length v)) #!unit)
          (else (begin
                  (vector-set! v m 0)
                  (strike! v (+ m n) n)))))

  ; lowest non-zero index of vector v >= n
  (define (nextprime v n)
    (if (zero? (vector-ref v n))
      (nextprime v (+ n 1))
      (vector-ref v n)))

  ; remove elements satisfying pred? from list lst
  (define (remove pred? lst)
    (cond
      ((null? lst) '())
      ((pred? (car lst))(remove pred? (cdr lst)))
      (else (cons (car lst) (remove pred? (cdr lst))))))

  ; the sieve itself
  (define (sieve n)
    (define stop (sqrt n))
    (define (iter v p)
      (cond
        ((> p stop) v)
        (else
          (begin
            (strike! v (* p p) p)
            (iter v (nextprime v (+ p 1)))))))

    (let ((v (make-vector (+ n 1))))
      (initialize! v)
      (vector-set! v 1 0) ; 1 is not a prime
      (remove zero? (vector->list (iter v 2)))))

  (sieve 100)))

; Taken from http://rosettacode.org/wiki/Sieve_of_Eratosthenes#Scheme
(define-test "stream sieve of Eratosthenes" (expect (13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
   ;;;; Stream Implementation
   (define (head s) (car s))
   (define (tail s) ((cdr s)))
   (define-syntax s-cons
     (syntax-rules () ((s-cons h t) (cons h (lambda () t)))))

   ;;;; Stream Utility Functions
   (define (from-By x s)
     (s-cons x (from-By (+ x s) s)))
   (define (take n s)
     (cond
       ((> n 1) (cons (head s) (take (- n 1) (tail s))))
       ((= n 1) (list (head s)))      ;; don't force it too soon!!
       (else '())))     ;; so (take 4 (s-map / (from-By 4 -1))) works
   (define (drop n s)
     (cond
       ((> n 0) (drop (- n 1) (tail s)))
       (else s)))
   (define (s-map f s)
     (s-cons (f (head s)) (s-map f (tail s))))
   (define (s-diff s1 s2)
     (let ((h1 (head s1)) (h2 (head s2)))
       (cond
         ((< h1 h2) (s-cons h1 (s-diff  (tail s1)       s2 )))
         ((< h2 h1)            (s-diff        s1  (tail s2)))
         (else                 (s-diff  (tail s1) (tail s2))))))
   (define (s-union s1 s2)
     (let ((h1 (head s1)) (h2 (head s2)))
       (cond
         ((< h1 h2) (s-cons h1 (s-union (tail s1)       s2 )))
         ((< h2 h1) (s-cons h2 (s-union       s1  (tail s2))))
         (else      (s-cons h1 (s-union (tail s1) (tail s2)))))))

   ;;;; join an ordered stream of streams (here, of primes' multiples)
   ;;;; into one ordered stream, via an infinite right-deepening tree
   (define (s-tree-join sts)                               ;; sts -> s
     (define (join-With of-Tail sts)                       ;; sts -> s
       (s-cons (head (head sts))
               (s-union (tail (head sts)) (of-Tail (tail sts)))))
     (define (pairs sts)                                   ;; sts -> sts
       (s-cons (join-With head sts) (pairs (tail (tail sts)))))
     (join-With (lambda (t) (s-tree-join (pairs t))) sts))


   ;;;; all primes' multiples are removed, merged through a tree of unions
   ;;;;  runs in ~ n^1.15 run time in producing n = 100K .. 1M primes
   (define (primes-stream)
     (define (mults p) (from-By (* p p) (* 2 p)))
     (define (no-mults-From from)
       (s-diff (from-By from 2)
               (s-tree-join (s-map mults odd-primes))))
     (define odd-primes
       (s-cons 3 (no-mults-From 5)))
     (s-cons 2 (no-mults-From 3)))

   (take 20 (drop 5 (primes-stream)))))

; Based on http://rosettacode.org/wiki/Factors_of_an_integer#Scheme
(define-test "integer factorization" (expect (1 239 4649 1111111)
  (import (llambda typed))

  (: factors (-> <exact-integer> <list-element>))
  (define (factors n)
    (: *factors (-> <exact-integer> <list-element>))
    (define (*factors d)
      (cond ((> d n) (list))
            ((= (truncate-remainder n d) 0) (cons d (*factors (+ d 1))))
            (else (*factors (+ d 1)))))
    (*factors 1))

  (factors 1111111)))

; Taken from http://rosettacode.org/wiki/Fibonacci_sequence#Scheme
(define-test "iterative fibonacci" (expect 144
  (define (fib-iter n)
    (do ((num 2 (+ num 1))
         (fib-prev 1 fib)
         (fib 1 (+ fib fib-prev)))
      ((>= num n) fib)))

  (fib-iter 12)))

; Based on http://rosettacode.org/wiki/Fibonacci_sequence#Scheme
(define-test "recursive fibonacci" (expect 144
  (import (llambda typed))

  (: fib-rec (-> <exact-integer> <exact-integer>))
  (define (fib-rec n)
    (if (< n 2)
      n
      (+ (fib-rec (- n 1))
         (fib-rec (- n 2)))))

  (fib-rec 12)))

; Based on http://rosettacode.org/wiki/Fibonacci_sequence#Scheme
(define-test "Dijkstra fibonacci" (expect 144
  (import (llambda typed))

  (: fib (-> <exact-integer> <exact-integer>))
  (define (fib n)
    (: fib-aux (-> <exact-integer> <exact-integer> <exact-integer> <exact-integer> <exact-integer> <exact-integer>))
    (define (fib-aux a b p q count)
      (cond ((= count 0) b)
            ((even? count)
             (fib-aux a
                      b
                      (+ (* p p) (* q q))
                      (+ (* q q) (* 2 p q))
                      (integer (/ count 2))))
            (else
              (fib-aux (+ (* b q) (* a q) (* a p))
                       (+ (* b p) (* a q))
                       p
                       q
                       (- count 1)))))
    (fib-aux 1 0 0 1 n))

  (fib 12)))

; Taken from http://rosettacode.org/wiki/Flatten_a_list#Scheme
(define-test "flatten a list" (expect (1 2 3 4 5 6 7 8)
  (define (flatten x)
  (cond ((null? x) '())
        ((not (pair? x)) (list x))
        (else (append (flatten (car x))
                      (flatten (cdr x))))))

  (flatten '((1) 2 ((3 4) 5) ((())) (((6))) 7 8 ()))))

; Taken from http://rosettacode.org/wiki/Longest_increasing_subsequence#Scheme
(define-test "patience sorting" (expect-success
  (define (lis less? lst)
    (define pile-tops (make-vector (length lst)))
    (define (bsearch-piles x len)
      (let aux ((lo 0)
                (hi (- len 1)))
        (if (> lo hi)
          lo
          (let ((mid (truncate-quotient (+ lo hi) 2)))
            (if (less? (car (vector-ref pile-tops mid)) x)
              (aux (+ mid 1) hi)
              (aux lo (- mid 1)))))))
    (let aux ((len 0)
              (lst lst))
      (if (null? lst)
        (reverse (vector-ref pile-tops (- len 1)))
        (let* ((x (car lst))
               (i (bsearch-piles x len)))
          (vector-set! pile-tops i (cons x (if (= i 0)
                                             '()
                                             (vector-ref pile-tops (- i 1)))))
          (aux (if (= i len) (+ len 1) len) (cdr lst))))))

  (assert-equal '(2 4 5) (lis < '(3 2 6 4 5 1)))
  (assert-equal '(0 2 6 9 11 15) (lis < '(0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15)))))

; Based on http://rosettacode.org/wiki/Levenshtein_distance#SchemeGG
(define-test "levenshtein distance" (expect 3
  (import (llambda typed))

  (: levenshtein (-> <string> <string> <exact-integer>))
  (define (levenshtein s t)
    (: %levenshtein (-> (Listof <char>) <exact-integer> (Listof <char>) <exact-integer> <exact-integer>))
    (define (%levenshtein s sl t tl)
      (cond ((zero? sl) tl)
            ((zero? tl) sl)
            (else
              (min (+ (%levenshtein (cdr s) (- sl 1) t tl) 1)
                   (+ (%levenshtein s sl (cdr t) (- tl 1)) 1)
                   (+ (%levenshtein (cdr s) (- sl 1) (cdr t) (- tl 1))
                      (if (char=? (car s) (car t)) 0 1))))))
    (%levenshtein (string->list s)
                  (string-length s)
                  (string->list t)
                  (string-length t)))

  (levenshtein "kitten" "sitting")))

; Taken from http://rosettacode.org/wiki/Permutations#Scheme
(define-test "permutations" (expect ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 2 1) (3 1 2))
  (define (perm s)
    (cond ((null? s) '())
          ((null? (cdr s)) (list s))
          (else ;; extract each item in list in turn and perm the rest
            (let splice ((l '()) (m (car s)) (r (cdr s)))
              (append
                (map (lambda (x) (cons m x)) (perm (append l r)))
                (if (null? r) '()
                  (splice (cons m l) (car r) (cdr r))))))))

  (perm '(1 2 3))))
