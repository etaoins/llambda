(define-test "empty quasiquote list" (expect ()
  `()))

(define-test "quasiquote list without splicing" (expect (1 2 3)
  `(1 2 3)))

(define-test "quasiquote list with non-splicing unquoting" (expect (1 5 4)
  `(1 ,(+ 2 3) 4)))

(define-test "quasiquote list with splicing unquoting mid-list" (expect (1 2 3 4)
  `(1 ,@(list 2 3) 4)))

(define-test "quasiquote list with splicing unquoting at end" (expect (1 2 3)
  `(1 ,@(list 2 3))))

(define-test "complex quasiquote list" (expect (1 2 3 4 5 6)
  `(1 2 ,@(list 3 4) ,(+ 2 3) 6)))

(define-test "empty quasiquote vector" (expect #()
  `#()))

(define-test "quasiquote vector without splicing" (expect #(1 2 3)
  `#(1 2 3)))

(define-test "quasiquote vector with non-splicing unquoting" (expect #(1 5 4)
  `#(1 ,(+ 2 3) 4)))

(define-test "quasiquote vector with splicing unquoting mid-vector" (expect #(1 2 3 4)
  `#(1 ,@(list 2 3) 4)))

(define-test "quasiquote vector with splicing unquoting at end" (expect #(1 2 3)
  `#(1 ,@(list 2 3))))

(define-test "complex quasiquote vector" (expect #(1 2 3 4 5 6)
  `#(1 2 ,@(list 3 4) ,(+ 2 3) 6)))
