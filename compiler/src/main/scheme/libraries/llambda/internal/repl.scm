(define-library (llambda internal repl)
  (import (scheme base))
  (import (llambda typed))
  (import (llambda nfi))

  (export write-stdout print-thunk-result)

  (begin
    (define write-stdout (native-function system-library "llcore_write_stdout" (-> <any> <unit>)))
    (define (print-thunk-result [thunk : (-> <any>)])
      (write-stdout (thunk)))))
