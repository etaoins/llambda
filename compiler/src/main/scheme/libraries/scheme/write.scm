(define-library (scheme write)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))
  (import (only (scheme base) current-output-port))

  ; write library
  (include-library-declarations "../../interfaces/scheme/write.scm")
  (begin
    (define native-write (world-function "lliby_write" (<any> <port>)))
    (define-r7rs write (case-lambda:
      (([datum : <any>])
       (native-write datum (current-output-port)))
      (([datum : <any>] [port : <port>])
       (native-write datum port))))

    (define native-display (world-function "lliby_display" (<any> <port>)))
    (define-r7rs display (case-lambda:
      (([datum : <any>])
       (native-display datum (current-output-port)))
      (([datum : <any>] [port : <port>])
       (native-display datum port)))))
)
