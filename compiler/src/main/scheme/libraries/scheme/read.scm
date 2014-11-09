(define-library (scheme read)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))
  (import (only (scheme base) current-input-port))

  (include-library-declarations "../../interfaces/scheme/read.scm")
  (begin
    (define native-read (world-function "lliby_read" (<port>) -> <any>))
    (define-r7rs read (case-lambda:
      (()
       (native-read (current-input-port)))
      (([port : <port>])
       (native-read port))))))
