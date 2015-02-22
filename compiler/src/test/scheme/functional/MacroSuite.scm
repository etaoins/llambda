(define-test "literal ellipsis" (expect-success
  (define-syntax be-like-begin
    (syntax-rules ()
                  ((be-like-begin name)
                   (define-syntax name
                     (syntax-rules ()
                                   ((name expr (... ...))
                                    (begin expr (... ...))))))))
  (be-like-begin sequence)
  (assert-equal 4 (sequence 1 2 3 4))))

(define-test "identifier bound to different value does not match" (expect-success
  (assert-equal 'ok (let ((=> #f))
                      (cond (#t => 'ok))))))

(define-test "defining syntax inside a (let)" (expect-success '(2 1)
  (let ((x 1) (y 2))
    (define-syntax swap!
      (syntax-rules ()
                    ((swap! a b)
                     (let ((tmp a))
                       (set! a b)
                       (set! b tmp)))))
    (swap! x y)
    (list x y))))

(define-test "syntax can expand to splicing (begin) in a body context" (expect-success
  (define-syntax add-defines
    (syntax-rules ()
                  ((add-defines bind1 bind2 val1 val2)
                   ; This form of (define) shouldn't introduce a new scope
                   (begin
                     (define bind1 val1)
                     (define bind2 val2)))))

  ; Test in outermost context
  (add-defines one two 1 2)
  (assert-equal 1 one)
  (assert-equal 2 two)

  ; And in body context
  (define (return-7)
    (add-defines three four 3 4)
    (+ three four))

  (assert-equal 7 (return-7))))
