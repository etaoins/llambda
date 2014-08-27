(define-test "parameters are procedures" (expect #t
	(define param (make-parameter 18))
	(procedure? param)))

(define-test "parameters return constant initial value if not parameterized" (expect 18
	(define param (make-parameter 18))
	(param)))

(define-test "parameters return non-constant initial value if not parameterized" (expect (1 2 3)
	(define param (make-parameter (list 1 2 3)))
	(param)))

(define-test "trivial parameterize" (expect 50
	(define param (make-parameter 0))

	(parameterize ((param 50))
		(param))))

(define-test "nested parameterize" (expect hello
	(define param (make-parameter 0))

	(parameterize ((param 50))
		(parameterize ((param 'hello))
			(param)))))

(define-test "multiple parameter parameterize" (expect (newOne two newThree) 
	(define param1 (make-parameter 'one))
	(define param2 (make-parameter 'two))
	(define param3 (make-parameter 'three))

	(parameterize ((param1 'newOne) (param3 'newThree))
		(list (param1) (param2) (param3)))))

(define-test "parameterize restores dynamic environment afterwards" (expect 0
	(define param (make-parameter 0))

	(parameterize ((param 50))
		(param))

	(param)))

(define-test "trivial dynamic-wind" (expect (56.1 . 60)
	(define testValue 0)

	(define returnValue
	  (dynamic-wind
		; Add 10 before the thunk - testValue will then be 10
		(lambda () (set! testValue (+ testValue 10)))
		; Multiply by 4 in the thunk and return 56.1 - testValue will then be 40
		(lambda () (set! testValue (* testValue 4)) 56.1)
		; Add 20 - testValue will then be 60
		(lambda () (set! testValue (+ testValue 20)))))

	(cons returnValue testValue)))

(define-test "continuations with (dynamic-wind)" (expect (connect talk1 disconnect connect talk2 disconnect)
  (let ((path '())
        (c #f))
    (let ((add (lambda (s)
                 (set! path (cons s path)))))
      (dynamic-wind
        (lambda () (add 'connect))
        (lambda ()
          (add (call-with-current-continuation
                 (lambda (c0)
                   (set! c c0)
                   'talk1))))
        (lambda () (add 'disconnect)))
      (if (< (length path) 4)
        (c 'talk2)
        (reverse path))))))

(define-test "continuations between two (dynamic-wind)s" (expect (
    first-before
    first-inner 
    first-after
    second-before
    second-inner
    second-after
    first-before
    first-after
    second-before
    second-inner
    second-after)
  (import (llambda typed))

  (define history-list '())

  (define first-cont #!unit)
  (define second-cont #!unit)

  (define: (append-history-item (item : <symbol>))
    (set! history-list (append history-list (list item))))

  (define is-recontinued
    (dynamic-wind
      (lambda () (append-history-item 'first-before))
      (lambda () 
        (call/cc (lambda (cont)
          (set! first-cont cont)
          (append-history-item 'first-inner)
          #f)))
      (lambda () (append-history-item 'first-after))))

  (dynamic-wind
    (lambda () (append-history-item 'second-before))
    (lambda () 
      (call/cc (lambda (cont)
        (set! second-cont cont)
        (append-history-item 'second-inner)
        ; This should leave our dynamic wind and re-enter the first one
        (unless is-recontinued
          (first-cont #t)))))
    (lambda () (append-history-item 'second-after)))
    
    history-list))
