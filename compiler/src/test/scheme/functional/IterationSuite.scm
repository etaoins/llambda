(define-test "(do) with two stepping variables" (expect 25
    (let ((x '(1 3 5 7 9)))
      (do ((x x (cdr x))
           (sum 0 (+ sum (car x))))
        ((null? x) sum)))))

(define-test "(do) with one stepping, one constant variable" (expect #(0 1 2 3 4)
    (do ((vec (make-vector 5 #!unit))
         (i 0 (+ i 1)))
      ((= i 5) vec)
      (vector-set! vec i i))))
