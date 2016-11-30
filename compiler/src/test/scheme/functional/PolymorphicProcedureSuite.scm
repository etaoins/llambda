(define-test "(cons) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-1 (typed-dynamic 1 <integer>))
  (define flonum-1 (typed-dynamic 1.0 <flonum>))

  (ann (cons integer-1 flonum-1) (Pairof <integer> <flonum>))))

(define-test "(car) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-1 (typed-dynamic 1 <integer>))
  (define flonum-1 (typed-dynamic 1.0 <flonum>))

  (ann (car (cons integer-1 flonum-1)) <integer>)))

(define-test "(cdr) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-1 (typed-dynamic 1 <integer>))
  (define flonum-1 (typed-dynamic 1.0 <flonum>))

  (ann (cdr (cons integer-1 flonum-1)) <flonum>)))

(define-test "(list) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-1 (typed-dynamic 1 <integer>))
  (define flonum-1 (typed-dynamic 1.0 <flonum>))

  (ann (list integer-1 flonum-1) (Listof <number>))))

(define-test "(memv) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-list (typed-dynamic '(1 2 4) (Listof <integer>)))
  (ann (memv 2 integer-list) (U #f (Listof <integer>)))))

(define-test "(member) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-list (typed-dynamic '(1 2 4) (Listof <integer>)))
  (ann (member 2 integer-list) (U #f (Listof <integer>)))))

(define-test "(assv) is polymorphic" (expect-success
  (import (llambda typed))

  (define symbol-to-int (typed-dynamic '((one . 1) (two . 2) (four . 4)) (Listof (Pairof <symbol> <integer>))))

  (ann (assv 'one symbol-to-int) (U #f (Pairof <symbol> <integer>)))))

(define-test "(list-tail) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-list (typed-dynamic '(1 2 4) (Listof <integer>)))
  (ann (list-tail integer-list 1) (Listof <integer>))))

(define-test "(list-ref) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-list (typed-dynamic '(1 2 4) (Listof <integer>)))
  (ann (list-ref integer-list 1) <integer>)))

(define-test "(reverse) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-list (typed-dynamic '(1 2 3) (Listof <integer>)))
  (define reverse-list (reverse integer-list))

  (ann integer-list (Listof <integer>))))

(define-test "(map) is polymorphic" (expect-success
  (import (llambda typed))

  (define flonum-1 (typed-dynamic 1.0 <flonum>))
  (define integer-list (typed-dynamic '(1 2 3) (Listof <integer>)))

  (ann (map (lambda (x) flonum-1) integer-list) (Listof <flonum>))))

(define-test "recursive polymorphic Scheme procedures" (expect-success
  (import (llambda typed))

  (: my-list-ref (All (A [N : <number>]) (-> (Listof A) N A)))
  (define (my-list-ref head count)
    (if (zero? count)
      (car head)
      (my-list-ref (cdr head) (- count 1))))

  (define int-result (my-list-ref '(1 2 3) 2))
  (ann int-result <integer>)
  (assert-equal 3 int-result)

  (define flonum-result (my-list-ref '(1.0 2.0 3.0) 1.0))
  (ann flonum-result <flonum>)
  (assert-equal 2.0 flonum-result)))

(define-test "(+) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-1 (typed-dynamic 1 <integer>))
  (define integer-2 (typed-dynamic 1 <integer>))

  (define flonum-1 (typed-dynamic 1.0 <flonum>))
  (define flonum-2 (typed-dynamic 2.0 <flonum>))

  (ann (+ integer-1 integer-2) <integer>)
  (ann (+ flonum-1 flonum-2) <flonum>)))

(define-test "(-) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-1 (typed-dynamic 1 <integer>))
  (define integer-2 (typed-dynamic 1 <integer>))

  (define flonum-1 (typed-dynamic 1.0 <flonum>))
  (define flonum-2 (typed-dynamic 2.0 <flonum>))

  (ann (- integer-1 integer-2) <integer>)
  (ann (- flonum-1 flonum-2) <flonum>)))

(define-test "(*) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-1 (typed-dynamic 1 <integer>))
  (define integer-2 (typed-dynamic 1 <integer>))

  (define flonum-1 (typed-dynamic 1.0 <flonum>))
  (define flonum-2 (typed-dynamic 2.0 <flonum>))

  (ann (* integer-1 integer-2) <integer>)
  (ann (* flonum-1 flonum-2) <flonum>)))

(define-test "(expt) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-1 (typed-dynamic 1 <integer>))
  (define integer-2 (typed-dynamic 1 <integer>))

  (define flonum-1 (typed-dynamic 1.0 <flonum>))
  (define flonum-2 (typed-dynamic 2.0 <flonum>))

  (ann (expt integer-1 integer-2) <integer>)
  (ann (expt flonum-1 flonum-2) <flonum>)))

(define-test "(square) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-1 (typed-dynamic 1 <integer>))
  (define flonum-1 (typed-dynamic 1.0 <flonum>))

  (ann (square integer-1) <integer>)
  (ann (square flonum-1) <flonum>)))

(define-test "(abs) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-1 (typed-dynamic 1 <integer>))
  (define flonum-1 (typed-dynamic 1.0 <flonum>))

  (ann (abs integer-1) <integer>)
  (ann (abs flonum-1) <flonum>)))

(define-test "(min) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-1 (typed-dynamic 1 <integer>))
  (define integer-2 (typed-dynamic 1 <integer>))

  (define flonum-1 (typed-dynamic 1.0 <flonum>))
  (define flonum-2 (typed-dynamic 2.0 <flonum>))

  (ann (min integer-1 integer-2) <integer>)
  (ann (min flonum-1 flonum-2) <flonum>)))

(define-test "(max) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-1 (typed-dynamic 1 <integer>))
  (define integer-2 (typed-dynamic 1 <integer>))

  (define flonum-1 (typed-dynamic 1.0 <flonum>))
  (define flonum-2 (typed-dynamic 2.0 <flonum>))

  (ann (max integer-1 integer-2) <integer>)
  (ann (max flonum-1 flonum-2) <flonum>)))

(define-test "(floor) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-1 (typed-dynamic 1 <integer>))
  (define flonum-1 (typed-dynamic 1.0 <flonum>))

  (ann (floor integer-1) <integer>)
  (ann (floor flonum-1) <flonum>)))

(define-test "(ceiling) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-1 (typed-dynamic 1 <integer>))
  (define flonum-1 (typed-dynamic 1.0 <flonum>))

  (ann (ceiling integer-1) <integer>)
  (ann (ceiling flonum-1) <flonum>)))

(define-test "(truncate) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-1 (typed-dynamic 1 <integer>))
  (define flonum-1 (typed-dynamic 1.0 <flonum>))

  (ann (truncate integer-1) <integer>)
  (ann (truncate flonum-1) <flonum>)))

(define-test "(round) is polymorphic" (expect-success
  (import (llambda typed))

  (define integer-1 (typed-dynamic 1 <integer>))
  (define flonum-1 (typed-dynamic 1.0 <flonum>))

  (ann (round integer-1) <integer>)
  (ann (round flonum-1) <flonum>)))

(define-test "simple polymorphic Scheme procedure" (expect-success
  (import (llambda typed))

  (: left-or-right (All (A) (-> <boolean> A A A)))
  (define (left-or-right use-left left-val right-val)
    (if use-left left-val right-val))

  (define integer-1 (typed-dynamic 1 <integer>))
  (define flonum-1 (typed-dynamic 1.0 <flonum>))

  (ann (left-or-right (dynamic-true) integer-1 flonum-1) <number>)))

(define-test "polymorphic Scheme procedure violating return type variable fails" (expect-compile-error type-error?
  (import (llambda typed))

  (: left-or-right (All (A) (-> <boolean> A A A)))
  (define (left-or-right use-left left-val right-val)
    'not-a-number)

  (define integer-1 (typed-dynamic 1 <integer>))
  (define flonum-1 (typed-dynamic 1.0 <flonum>))

  (left-or-right (dynamic-true) integer-1 flonum-1)))

(define-test "violating Scheme procedure's type bounds fails" (expect-compile-error type-error?
  (import (llambda typed))

  (: left-or-right (All ([A : <number>]) (-> <boolean> A A A)))
  (define (left-or-right use-left left-val right-val)
    (if use-left left-val right-val))

  (define integer-1 (typed-dynamic 1 <integer>))
  (define string-val (typed-dynamic "Hello" <string>))

  (left-or-right (dynamic-true) integer-1 string-val)))

(define-test "capturing polymorphic Scheme procedure" (expect-success
  (import (llambda typed))

  (define outer-counter 0)

  (: inc-if-string (All (A) (-> A <unit>)))
  (define (inc-if-string possible-string)
    (when (string? possible-string)
      (set! outer-counter (+ outer-counter 1))))

  (define integer-1 (typed-dynamic 1 <integer>))
  (define flonum-1 (typed-dynamic 1.0 <flonum>))
  (define string-val (typed-dynamic "Hello" <string>))

  (inc-if-string integer-1)
  (inc-if-string flonum-1)
  (inc-if-string string-val)

  (assert-equal 1 outer-counter)))

(define-test "converting polymorphic procedures to specific procedure types" (expect-success
  (import (llambda typed))

  (: return-arg (All (A) (-> A A)))
  (define (return-arg x) x)

  (: int-mapper (-> <integer> (-> <integer> <integer>) <integer>))
  (define (int-mapper val mapper)
    (mapper val))

  ; The expectation here is that the compiler will instantiate the exact polymorph requested. There is no easy way to
  ; verify this as the upper bound + trampoline will behave the same way. The best we can do is make sure this is
  ; semantically correct.
  (assert-equal 5 (int-mapper 5 return-arg))))

(define-test "polymorphic procedure in inner scope" (expect-success
  (import (llambda typed))

  (assert-equal 5
                (begin
                  (: return-self (All (A) (-> A A)))
                  (define (return-self x) x)

                  (return-self 5)))))

(define-test "polymorphic procedure taking parameterized procedure of wrong type fails at compile time" (expect-compile-error type-error?
  (import (llambda typed))

  (: apply-single (All (A B) (-> (-> A B) A B)))
  (define (apply-single proc val)
    (proc val))

  (apply-single symbol->string "Hello")))

(define-test "applying a polymorphic procedure checks its return type" (expect-error type-error?
  (import (llambda typed))

  (: halve-number (All ([A : <number>]) (-> A A)))
  (define (halve-number n)
    (/ n 2))

  (halve-number 3)))
