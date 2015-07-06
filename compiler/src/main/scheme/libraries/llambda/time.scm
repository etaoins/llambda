(define-library (llambda time)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))
  (import (scheme time))

  (include-library-declarations "../../interfaces/scheme/time.scm")
  (export current-unix-time)

  (begin
    (define-native-library lltime (static-library "ll_llambda_time"))

    (define current-unix-time (native-function lltime "lltime_current_unix_time" (-> <native-double>)))))
