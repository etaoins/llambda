(define-test "non-parameterized parameters" (expect-success
  (assert-equal 'foo ((make-parameter 'foo)))

  (define static-val-param (make-parameter 5))
  (assert-true (procedure? static-val-param))
  (assert-equal 5 (static-val-param))

  (define dynamic-val-param (make-parameter (list (typed-dynamic 10 <integer>))))
  (assert-true (procedure? dynamic-val-param))
  (assert-equal '(10) (dynamic-val-param))))

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

(define-test "parameters cannot be cast to the value they contain" (expect-compile-error type-error?
  (import (llambda typed))
  (define param : <integer> (make-parameter 5))))

(define-test "multiple parameter parameterize" (expect (newOne two newThree)
  (define param1 (make-parameter 'one))
  (define param2 (make-parameter 'two))
  (define param3 (make-parameter 'three))
  (define param4 (make-parameter 'four))
  (define param5 (make-parameter 'five))

  (parameterize ((param1 'newOne) (param3 'newThree) (param4 'newFour))
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

(define-test "recursive parameter definition" (expect-success
  (define param (make-parameter (lambda () param)))
  (define inner-param ((param)))

  (assert-equal param inner-param)))
