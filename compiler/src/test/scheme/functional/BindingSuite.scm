(define-test "simple let*" (expect 70
	; This is taken from R7RS
	(let ((x 2) (y 3))
	  (let* ((x 7)
			 (z (+ x y)))
		(* z x)))))

(define-test "recursive function definition in lambda body" (expect #t
  ((lambda ()
     (define even?
     (lambda (n)
       (if (zero? n)
       #t
       (odd? (- n 1)))))
     
     (define odd?
     (lambda (n)
       (if (zero? n)
       #f
       (even? (- n 1)))))

     (even? 8)
  ))))

(define-test "recursive mutable function definition in lambda body" (expect #t
  ((lambda ()
     (define even?
     (lambda (n)
       (if (zero? n)
       #t
       (odd? (- n 1)))))
     
     (define odd?
     (lambda (n)
       (if (zero? n)
       #f
       (even? (- n 1)))))
	 
     ; Setting this to a procedure returning true means (even?) will always return true
     (set! odd? (lambda (n) #t))
     (even? 7)
  ))))

(define-test "accessing recursive define before initialization fails" (expect-failure
  ((lambda ()
     (define even?
     (lambda (n)
       (if (zero? n)
       #t
       (odd? (- n 1)))))

     (define five-is-odd (odd? 5))
     
     (define odd?
     (lambda (n)
       (if (zero? n)
       #f
       (even? (- n 1)))))
  ))))
