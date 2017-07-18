(define-library (scheme write)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-stdlib-procedure define-stdlib)))
  (import (only (scheme base) current-output-port))

  (export write display)

  (begin
    (define-native-library llwrite (static-library "ll_scheme_write"))

    (define native-write (world-function llwrite "llwrite_write" (-> <any> <port> <unit>)))
    (define-stdlib (write [datum : <any>] [port : <port> (current-output-port)])
                 (native-write datum port))

    (define native-display (world-function llwrite "llwrite_display" (-> <any> <port> <unit>)))
    (define-stdlib (display [datum : <any>] [port : <port> (current-output-port)])
                 (native-display datum port))))
