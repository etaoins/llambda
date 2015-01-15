(define-library (llambda internal repl)
  (import (scheme base))
  (import (scheme write))
  (import (llambda typed))

  (export print-thunk-result)

  (begin
    ; This is used to pretty print results inside the REPL. This works like (write) unless multiple values are provided.
    (define (print-thunk-result [thunk : (-> *)])
      (call-with-values thunk
                        (lambda vals
                          (if (= 1 (length vals))
                            ; Only one value
                            (write (car vals))
                            ; Multiple values
                            (write `(values ,@vals))))))))
