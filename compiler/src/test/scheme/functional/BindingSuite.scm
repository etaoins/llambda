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

(define-test "hygienic scoped macro binding using let-syntax" (expect (now . outer)
  (define result1 (let-syntax ((given-that (syntax-rules ()
										 ((given-that test stmt1 stmt2 ...)
										  (if test
											(begin stmt1
												   stmt2 ...))))))
	; This is also a s)eaky hygiene test from R7RS
	; "if" is just a normal variable in the below code
	; Overriding it must not interfere with the original report "if" in the macro above once its expanded
	(let ((if #t))
	  (given-that if (set! if 'now))
	  if)))
  
  (define result2 (let ((x 'outer))
	(let-syntax ((m (syntax-rules () ((m) x))))
	  (let ((x 'inner))
		(m)))))
  (cons result1 result2))) 

(define-test "simple letrec*" (expect 5
  (letrec* ((p
              (lambda (x)
                (+ 1 (q (- x 1)))))
            (q
              (lambda (y)
                (if (zero? y)
                  0
                  (+ 1 (p (- y 1))))))
            (x (p 5))
            (y x))
           y)))

(define-test "simple letrec" (expect #t
  (letrec ((even?
             (lambda (n)
               (if (zero? n)
                 #t
                 (odd? (- n 1)))))
           (odd?
             (lambda (n)
               (if (zero? n)
                 #f
                 (even? (- n 1))))))
    (even? 8))))
