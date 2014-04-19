(define-test "(cond) without arrows or else" (expect true 
	(cond (#f 'false)
			(#t 'true))))

(define-test "(cond) with arrows, without else" (expect #f
	(cond (#f => not)
			; This becomes (not #t)
			(#t => not))))

(define-test "(cond) without arrows, with else" (expect else
	(cond (#f 'false1)
			(#f 'false2)
			(else 'else))))

(define-test "(cond) with arrows and else" (expect else
	(cond (#f => not)
			(#f => not)
			(else 'else))))

(define-test "(case) matching clause" (expect composite
	(case (* 2 3)
	  ((2 3 5 7) 'prime)
	  ((1 4 6 8 9) 'composite))))

(define-test "(case) without matching clause" (expect #!unit
	(case (car '(c d)) 
	  ((a) 'a)
	  ((b) 'b))))

(define-test "(case) with arrow" (expect c
	(case (car '(c d))
	  ((a e i o u) 'vowel)
	  ((w y) 'semivowel)
	  (else => (lambda (x) x)))))

(define-test "empty (and) evaluates to true" (expect #t
	(and)))

(define-test "(and #t #f) is false" (expect #f
	(and #t #f)))

(define-test "(and) returns the last evaluated datum" (expect (f g)
	(and 1 2 'c '(f g))))

(define-test "empty (or) evaluates to false" (expect #f
	(or)))

(define-test "(or #t #f) is true" (expect #t
	(or #t #f)))

(define-test "(or) returns the last evaluated datum" (expect (b c)
	(or #f '(b c) #t)))

(define-test "(when) with true condition" (expect executed
	(define result 'not-executed)
	(when (= 1 1.0)
	  (set! result 'executed))
	result))

(define-test "(when) with false condition" (expect not-executed
	(define result 'not-executed)
	(when (= 1 6)
	  (set! result 'executed))
	result))

(define-test "(unless) with true condition" (expect not-executed
	(define result 'not-executed)
	(unless (= 1 1.0)
	  (set! result 'executed))
	result))

(define-test "(unless) with false condition" (expect executed
	(define result 'not-executed)
	(unless (= 1 6)
	  (set! result 'executed))
	result))

(define-test "conditional with garbage collection in only one branch" (expect true
	(import (llambda test-util))
   (define result 'not-executed)

   (if undecided-true
     (begin
       (cons 1 2)
       (set! result 'true))
     (begin 
       (set! result 'false)))

   result))
