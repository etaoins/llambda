(define-test "+ is a procedure" (expect #t
  (procedure? +)))

(define-test "#f is not a procedure" (expect #f
  (procedure? #f)))

(define-test "R7RS procedure examples" (expect-success
  (assert-true (procedure? (lambda (x) (+ x x))))
  (assert-equal 8 ((lambda (x) (+ x x)) 4))

  (define reverse-subtract (lambda (x y) (- y x)))
  (assert-equal 3 (reverse-subtract 7 10))

  (define add4
      (let ((x 4))
        (lambda (y) (+ x y))))

  (assert-equal 10 (add4 6))

  (assert-equal '(3 4 5 6) ((lambda x x) 3 4 5 6))
  (assert-equal '(5 6) ((lambda (x y . z) z) 3 4 5 6))))

(define-test "static self-executing lambdas" (expect-static-success
  (assert-equal 15 ((lambda () 15)))
  (assert-equal 25 ((lambda (arg) arg) 25))
  (assert-equal #t ((lambda rest-args (null? rest-args))))))

(define-test "static bound procedures" (expect-static-success
  (define (count-rest-args . rest-args)
    (length rest-args))
  (assert-equal 5 (count-rest-args 1 2 3 4 5))

  (define (add-first-2 . rest-args)
    (+ (car rest-args) (car (cdr rest-args))))
  (assert-equal 3 (add-first-2 1 2 3 4 5))

  ; This has to recursively inline
  (define (add-two n)
    (+ 2 n))
  (define (times-four n)
    (* (add-two 2) n))
  (assert-equal 32 (times-four 8))

  (assert-equal 15
    (let ((return-15 (lambda () 15)))
      (return-15)))

  (assert-equal -10
    (let ((return-arg (lambda (arg) arg)))
      (return-arg -10)))

  (assert-equal #t
    (let ((has-no-args? (lambda rest-args (null? rest-args))))
      (has-no-args?)))

  ; This needs to capture val-times-two during inlining from the parent scope
  (define (capturing-times-four val)
    (define (val-times-two) (+ val val))
    (+ (val-times-two) (val-times-two)))

  (assert-equal -24 (capturing-times-four -6))))

(define-test "static procedure values" (expect-static-success
   (define (add2 val) (+ val 2))
   (define (apply-proc-to-arg proc arg)
     (proc arg))
   (assert-equal 8 (apply-proc-to-arg add2 6))

   (define proc-from-let (let ((mutliplier 2))
     (lambda (to-multiply)
       (* mutliplier to-multiply))))
   (assert-equal -10 (proc-from-let -5))

   (define (multi-op val1 val2 use-minus)
     (define operator (if use-minus - +))
     (operator val1 val2))
   (multi-op 4 10 #f)))

(define-test "procedures accepting procedure arguments are procedures" (expect-success
  (import (llambda typed))

  (: higher-level-proc (-> (-> <number> <number> <number>) <number> <number>))
  (define (higher-level-proc op val)
    (op 2 val))

  (procedure? higher-level-proc)))

(define-test "non-constant procedure values can be applied" (expect 10
  (define math-op
    ; This is + but our optimiser won't know that
    ; That means this has to be converted to a function value at runtime

    ; Also note that + and / have different signatures which will have to be
    ; normalized when they're boxed. This doesn't matter for our current
    ; implementation but it may matter if we try to be more tricky
    (if (dynamic-true) + /))

  (math-op 2 3 5)))

(define-test "datum cells can be applied" (expect 10
  ((typeless-cell -) 80 50 20)))

(define-test "datum cells can be converted to specific procedure type" (expect 8
  (import (llambda typed))

  (: double-binary-op (-> (-> <exact-integer> <exact-integer> <exact-integer>) <exact-integer> <exact-integer>))
  (define (double-binary-op proc arg)
    (proc arg arg))

  (double-binary-op (typeless-cell +) 4)))

(define-test "specific procedure types can be converted to other procedure types" (expect 8
  (import (llambda typed))

  (define plus : (-> <number> * <number>) *)
  (set! plus +)

  (: double-binary-op (-> (-> <exact-integer> <exact-integer> <exact-integer>) <exact-integer> <exact-integer>))
  (define (double-binary-op proc arg)
    (proc arg arg))

  (double-binary-op plus 4)))

(define-test "multiple specific procedure types can be converted to the same other procedure type" (expect-success
  (import (llambda typed))

  (define plus : (-> <number> * <number>) *)
  (set! plus +)

  (define construct : (-> <number> <number> <any>) *)
  (set! construct cons)

  (: double-binary-op (-> (-> <exact-integer> <exact-integer> <any>) <exact-integer> <any>))
  (define (double-binary-op proc arg)
    (proc arg arg))

  (assert-equal 8 (double-binary-op plus 4))
  (assert-equal '(4 . 4) (double-binary-op construct 4))))

(define-test "unions containing procedure types can be converted to other procedure types" (expect 8
  (import (llambda typed))

  (define plus : (U <unit> (-> <number> * <number>)) #!unit)
  (set! plus +)

  (: double-binary-op (-> (-> <exact-integer> <exact-integer> <exact-integer>) <exact-integer> <exact-integer>))
  (define (double-binary-op proc arg)
    (proc arg arg))

  (double-binary-op plus 4)))

(define-test "applying datum cells with too many arguments fails" (expect-error arity-error?
  ((typeless-cell exact?) 80 20)))

(define-test "applying datum cells with insufficient arguments fails" (expect-error arity-error?
  ((typeless-cell exact?))))

(define-test "phied number procedure cannot be applied with non-number arguments" (expect-compile-error type-error?
  ; + and - have different signatures
  ; However, we should determine that this is impossible because their phied type should be (-> <number> * <number>)
  (define math-proc (if (dynamic-true) + -))
  (math-proc 'one 'two)))

(define-test "procedure returning nothing" (expect #!unit
  (define (return-nothing))
  (return-nothing)))

(define-test "procedure returning single value" (expect 7
  (define (return-7) 7)
  (return-7)))

(define-test "typed procedure with mismatched return type fails" (expect-error type-error?
  (import (llambda typed))

  (: bad-proc (-> <number> <string>))
  (define (bad-proc x)
    'symbol)

  (bad-proc)))

(define-test "untyped procedure returning its only argument" (expect 7
  (define (return-value value) value)
  (return-value 7)))

(define-test "typed procedure returning its only argument" (expect 7
  (import (llambda typed))
  (define (return-value (value : <exact-integer>)) value)
  (return-value 7)))

(define-test "typed procedure invoked with wrong type fails" (expect-error type-error?
  (import (llambda typed))
  (define (return-value (value : <exact-integer>)) value)
  (return-value 'symbol)))

(define-test "typed procedure invoked with wrong rest arg type fails" (expect-error type-error?
  (import (llambda typed))
  (define (return-value vals : <exact-integer> *)
    vals)

  (return-value 'symbol)))

(define-test "typed procedure invoked as datum cell with wrong rest arg type fails" (expect-error type-error?
  (import (llambda typed))
  (define (return-value vals : <exact-integer> *)
    vals)

  ((typeless-cell return-value) 'symbol)))

(define-test "procedure with rest arg can be converted to typed procedure without rest arg" (expect 1
  (import (llambda typed))

  (: apply-thunk (-> (-> <any>) <any>))
  (define (apply-thunk thunk)
    (thunk))

  (apply-thunk *)))

(define-test "typed procedure invoked with correct rest arg types" (expect-success
  (import (llambda typed))
  (define (add-values vals : <exact-integer> *)
    (cond-expand (immutable-pairs
      (ann vals (Listof <exact-integer>))))

    (apply + vals))

  (assert-equal 6 (add-values 1 2 3))))

(define-test "untyped procedure adding its arguments" (expect 7
  (define (add-two-values a b) (+ a b))
  (add-two-values 4 3)))

(define-test "typed procedure adding its arguments" (expect 7
  (import (llambda typed))
  (define (add-two-values [a : <exact-integer>] [b : <exact-integer>]) (+ a b))
  (add-two-values 4 3)))

(define-test "procedure returning value can be passed as procedure returning unit" (expect finished
  (import (llambda typed))

  (: invoke-unit-thunk (-> <exact-integer> <exact-integer> (-> <exact-integer> <exact-integer> <unit>) <unit>))
  (define (invoke-unit-thunk val1 val2 thunk)
    (thunk val1 val1)
    'finished)

  (invoke-unit-thunk 3 4 +)))

(define-test "procedure mutating args does not affect caller" (expect 7
  (define (set-arg a) (set! a -89))

  (define outer-value 7)
  (set-arg outer-value)
  outer-value))

(define-test "procedure mutating rest arg itself" (expect 3
    (define (set-rest-arg . rest)
      (set! rest '(1 2 3))
      (length rest))

    (set-rest-arg 1 2 3 4 5)))

(cond-expand ((not immutable-pairs)
  ; R7RS requires the procedure to be passed a newly allocated list
  (define-test "procedure mutating rest list contents" (expect (3 2)
      (define (mutate-rest-arg . rest)
        (set-car! rest 3)
        rest)

      (mutate-rest-arg 1 2)))
  (define-test "procedure mutating rest list to improper list" (expect-success
      (define (mutate-rest-arg . rest)
        (set-car! rest 3)
        (set-cdr! rest 4)
        ; No longer an proper list
        (assert-false (list? rest))
        rest)

      (assert-equal '(3 . 4) (mutate-rest-arg 1 2))))))

(define-test "capturing constants" (expect 7
  (define two 2)

  (define (add-two value)
    (+ two value))

  (add-two 5)))

(define-test "capturing immutable variables" (expect 7
  (define (create-adder-proc to-add)
    (lambda (value)
     (+ to-add value)))

  (define add-five (create-adder-proc 5))
  (add-five 2)))

(define-test "capturing mutable variables" (expect (1 2 1 3 2 3)
  (import (llambda typed))

  (define (create-counter)
    (define value : <exact-integer> 0)
    (lambda ()
     (set! value (+ value 1))
     value))

  (define first-counter (create-counter))
  (define second-counter (create-counter))

  (define first-value (first-counter))  ; 1
  (define second-value (first-counter)) ; 2
  (define third-value (second-counter)) ; 1
  (define fourth-value (first-counter)) ; 3
  (define fifth-value (second-counter)) ; 2
  (define sixth-value (second-counter)) ; 3


  (list first-value second-value third-value fourth-value fifth-value sixth-value)))

(define-test "setting mutable variable to procedure argument" (expect 10
  (define outer-value 0)
  (apply
    (lambda (value-to-set)
     (set! outer-value value-to-set))
    '(10))

  outer-value))

(define-test "capturing another lambda" (expect (2 4 6)
  (define (create-multiplier factor)
    (lambda (value)
     (* factor value)))

  (define (create-transformed-counter transformer)
    (define value 0)
    (lambda ()
     (set! value (+ value 1))
     (transformer value)))

  ; Create a transformer
  (define times-two (create-multiplier 2))
  ; Pass the transformer to our counter
  (define counter (create-transformed-counter times-two))

  (define first-value (counter))
  (define second-value (counter))
  (define third-value (counter))

  (list first-value second-value third-value)))

(define-test "recursive lambda mutating itself" (expect 15
  (letrec ((false-loop
             (lambda (val)
               ; Overwrite ourselves
               (set! false-loop *)
               ; This should invoke *
               (false-loop val 5))))
    (false-loop 3))))

(define-test "procedure taking typed procedure argument" (expect 50
  (import (llambda typed))
  (: apply-number-proc (-> (-> <number> <number> <number>) <exact-integer> <exact-integer> <number>))
  (define (apply-number-proc binary-op val1 val2)
    (binary-op val1 val2))

  (apply-number-proc * -25 -2)))

(define-test "procedure taking typed procedure argument of wrong type fails" (expect-compile-error type-error?
  (import (llambda typed))
  (: apply-number-proc (-> (-> <number> <number> <number>) <exact-integer> <exact-integer> <number>))
  (define (apply-number-proc binary-op val1 val2)
    (binary-op val1 val2))

  (apply-number-proc list -25 -2)))

(define-test "typed procedure returning typed, recursive, capturing procedure" (expect-success
  (import (llambda typed))

  (: make-length-counter (-> <boolean> (-> <any> <exact-integer>)))
  (define (make-length-counter fail-on-improper)
    (: manual-length (-> <any> <exact-integer>))
    (define (manual-length obj)
      (if (pair? obj)
        (+ (manual-length (cdr obj)) 1)
        (if (and fail-on-improper (not (null? obj)))
          (raise "Improper list!")
          0)))

      manual-length)

    (assert-equal 5 ((make-length-counter (dynamic-false)) '(1 2 3 4 5 . 6)))
    (assert-raises string?
      ((make-length-counter (dynamic-true)) '(1 2 3 4 5 . 6)))))

(cond-expand
  (immutable-pairs
    (define-test "applying typed procedure rest argument" (expect-success
      (import (llambda typed))

      ; Takes an initial value and then four numerical procedures to fold the value over
      (: apply-number-procs (-> <number> (-> <number> <number>) * <number>))
      (define (apply-number-procs initial .  proc-rest-arg)
        (define first-proc (car proc-rest-arg))
        (define second-proc (car (cdr proc-rest-arg)))
        (define third-proc (car (cdr (cdr proc-rest-arg))))
        (define fourth-proc (car (cdr (cdr (cdr proc-rest-arg)))))

        (fourth-proc (third-proc (second-proc (first-proc initial)))))

      (assert-equal -0.25 (apply-number-procs 4 - * + /))))))

(define-test "invoking procedures placed in lists" (expect-success
  (define escape-list
    ((lambda (mutable-arg)
      ; This exists to prevent compile-time evaluation
      (set! mutable-arg #f)
      (list (lambda ()))) #!unit))

  (define escaped-lambda (car escape-list))
  (escaped-lambda)))

(define-test "invoking procedures returned from multiple value lists" (expect-output (-1)
  (import (scheme write))
  (import (scheme process-context))

  (call-with-values
    (lambda ()
      (values
        (if (dynamic-true) write exit)
        (if (dynamic-false) + -)))
    (lambda (writer proc)
      (writer (proc 1 2))))))

(define-test "trivial typed tail recursion" (expect ()
  (import (llambda typed))

  ; This is non-allocating which means it should complete in reasonable time even with GC debugging enabled
  (: list-terminator (-> <list-element> <any>))
  (define (list-terminator head)
    (if (pair? head)
      (list-terminator (cdr head))
      head))

  (list-terminator (make-list 500000))))

(define-test "trivial untyped tail recursion" (expect ()
  (define (list-terminator head)
    (if (pair? head)
      (list-terminator (cdr head))
      head))

  (list-terminator (make-list 500000))))

(define-test "tail recursion with a counter" (expect (3)
  (import (llambda typed))

  (: my-list-tail (-> <list-element> <exact-integer> <any>))
  (define (my-list-tail head count)
    (if (zero? count)
      head
      (my-list-tail (cdr head) (- count 1))))

  (my-list-tail '(1 2 3) 2)))

(define-test "procedures taking special argument names" (expect-success
  ; This contains whitespace
  (define (takes-whitespace |Return Me!|) |Return Me!|)

  (assert-equal 'success (takes-whitespace 'success))
  (assert-equal 'success ((typeless-cell takes-whitespace) 'success))

  ; This contains a comma
  (define (takes-comma |return,me|) |return,me|)

  (assert-equal 'success (takes-comma 'success))
  (assert-equal 'success ((typeless-cell takes-comma) 'success))

  (define captured 0)

  ; self is the internal argument name for procedures taking their own closure
  (define (takes-self self)
    (set! captured (+ captured 1))
    self)

  (assert-equal 'success (takes-self 'success))
  (assert-equal 'success ((typeless-cell takes-self) 'success))

  ; world is the internal argument name for procedures that require access to the current world
  (define (takes-world world)
    (make-parameter "This needs a world pointer")
    world)

  (assert-equal 'success (takes-world 'success))
  (assert-equal 'success ((typeless-cell takes-world) 'success))))
