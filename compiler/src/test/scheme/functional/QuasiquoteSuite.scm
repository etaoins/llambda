(define-test "quasiquote list without splicing" (expect (1 2 3)
	`(1 2 3)))

(define-test "quasiquote list with non-splicing unquoting" (expect (1 5 4)
	`(1 ,(+ 2 3) 4)))

(define-test "quasiquote vector without splicing" (expect #(1 2 3)
	`#(1 2 3)))

(define-test "quasiquote vector with non-splicing unquoting" (expect #(1 5 4)
	`#(1 ,(+ 2 3) 4)))
