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

(define-test "parameter procedures don't accept arguments" (expect-error arity-error?
  ((make-parameter 50) #t)))

(define-test "parameterize with non-parameter fails" (expect-error invalid-argument-error?
  (define not-param +)

  (parameterize ((not-param 50))
    (not-param))))

(define-test "nested parameterize" (expect hello
  (define param (make-parameter 0))

  (parameterize ((param 50))
    (parameterize ((param 'hello))
      (param)))))

(define-test "parameters can be cast to <procedure>" (expect-success
  (import (llambda error))

  (define param (typed-dynamic (make-parameter 'hello) <procedure>))

  (assert-equal 'hello (param))
  (assert-equal 'world (parameterize ((param 'world)) (param)))
  (assert-raises arity-error?
                 (param 10))))

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

(define-test "procedures do not capture their parameter values" (expect 0
  (define param (make-parameter 0))

  (define return-param-value
    (parameterize ((param 50))
      (lambda () (param))))

  (return-param-value)))

(define-test "procedures can be parameterized" (expect world
  (define param (make-parameter (lambda () 'hello)))

  (parameterize ((param (lambda () 'world)))
    ((param)))))
