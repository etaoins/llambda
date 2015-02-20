(define-library (llambda internal repl)
  (import (scheme base))
  (import (llambda typed))
  (import (llambda nfi))

  (export write-stdout print-thunk-result)

  (begin
    (define write-stdout (native-function system-library "llcore_write_stdout" (-> <any> <unit>)))

    ; This is used to pretty print results inside the REPL. This works like (write) unless multiple values are provided.
    (define (print-thunk-result [thunk : (-> *)])
      (call-with-values thunk
                        (lambda vals
                          (if (= 1 (length vals))
                            ; Only one value
                            (write-stdout (car vals))
                            ; Multiple values
                            (write-stdout `(values ,@vals))))))))
