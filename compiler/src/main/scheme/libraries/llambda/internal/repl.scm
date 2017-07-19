(define-library (llambda internal repl)
  (import (llambda base))
  (import (llambda typed))
  (import (llambda nfi))

  (export write-stdout)

  (begin
    (define write-stdout (native-function system-library "llcore_write_stdout" (-> <any> <unit>) nocapture))))
