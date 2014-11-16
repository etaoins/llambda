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
