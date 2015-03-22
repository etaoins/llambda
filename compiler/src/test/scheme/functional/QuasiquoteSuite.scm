(define-test "quasiquote list" (expect-static-success
  (assert-equal '() `())
  (assert-equal '(1 2 3) `(1 2 3))
  (assert-equal '(1 5 4) `(1 ,(+ 2 3) 4))
  (assert-equal '(1 2 3 4) `(1 ,@(list 2 3) 4))
  (assert-equal '(1 2 3) `(1 ,@(list 2 3)))
  (assert-equal '(1 2 3 4 5 6) `(1 2 ,@(list 3 4) ,(+ 2 3) 6))))

(define-test "quasiquote vector" (expect-success
  (assert-equal #() `#())
  (assert-equal #(1 2 3) `#(1 2 3))
  (assert-equal #(1 5 4) `#(1 ,(+ 2 3) 4))
  (assert-equal #(1 2 3 4) `#(1 ,@(list 2 3) 4))
  (assert-equal #(1 2 3) `#(1 ,@(list 2 3)))
  (assert-equal #(1 2 3 4 5 6) `#(1 2 ,@(list 3 4) ,(+ 2 3) 6))))
