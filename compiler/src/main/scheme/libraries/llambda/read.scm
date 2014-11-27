(define-library (llambda read)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))
  (import (only (scheme base) current-input-port))

  (include-library-declarations "../../interfaces/scheme/read.scm")
  (export <readable>)

  (begin
    (define-type <readable> (U <pair> <empty-list> <string> <symbol> <boolean> <number> <char> <vector> <bytevector>
                               <unit>))

    (define-native-library llread (static-library "llread"))

    (define native-read (world-function llread "llread_read" (-> <port> (U <readable> <eof-object>))))
    (define-r7rs read (case-lambda
      (()
       (native-read (current-input-port)))
      (([port : <port>])
       (native-read port))))))
