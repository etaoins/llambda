(define-test "static optional args" (expect-static-success
  (import (llambda typed))

  (define (trivial-optional [value 5])
    (assert-equal 5 value))

  (trivial-optional)
  (trivial-optional 5)

  (define (rest-optional [value 5] . rest-args)
    (assert-equal 1 value)
    (assert-equal '(2 3) rest-args))

  (rest-optional 1 2 3)
  (apply rest-optional '(1 2 3))

  (define (proc-optional arg1 arg2 [op +])
    (op arg1 arg2))

  (assert-equal 10 (proc-optional 5 5))
  (assert-equal 0 (proc-optional 5 5 -))

  (define (multi-optional [a (+ 0 1)] [b (+ a 1)] [c (+ b 1)] . rest)
    (append (list a b c) rest))

  (assert-equal '(1 2 3) (multi-optional))
  (assert-equal '(4 5 6 7 8 9) (multi-optional 4 5 6 7 8 9))

  (define (typed-optional [value : <integer> 10]) value)

  (assert-equal 10 (typed-optional))
  (assert-equal 5 (typed-optional 5))

  (assert-equal 10 (apply typed-optional))
  (assert-equal -10 (apply typed-optional '(-10)))))

(define-test "optional arg with incompatible type fails" (expect-compile-error type-error?
  (import (llambda typed))

  (define (typed-optional [val : <integer> #t]) val)

  (typed-optional)))

(define-test "optional args can be converted to procedures taking more arguments" (expect-success
  (define (add-value op1 [op2 5]) (+ op1 op2))

  (assert-equal '(6 7 8 9) (map add-value '(1 2 3 4)))))

(define-test "optional args can be converted to procedures taking fewer arguments" (expect-success
  (import (llambda list))
  (import (llambda typed))

  (define (multiply-value val [factor : <integer> 2])
    (* val factor))

  (assert-equal '(0 2 4) (list-tabulate 3 multiply-value))))

(define-test "optional args can throw exceptions" (expect-success
  (import (llambda error))

  (define (throw-if-missing [val (raise-no-actor-error "TEST")]) val)

  (assert-raises no-actor-error?
                 (throw-if-missing))))

(define-test "procedures can be passed in optional args" (expect-success
  (import (llambda typed))

  (define (call-thunk [thunk : (-> <integer>) (lambda () 0)])
    (thunk))

  (assert-equal 100 (call-thunk (lambda () 100)))))

(define-test "procedures taking optional args can be passed as optional args" (expect-success
  (import (llambda typed))

  (define (call-thunk [thunk : (-> <integer>) (lambda () 0)])
    (thunk))

  (assert-equal 100 (call-thunk (lambda ([val 100]) val)))))

