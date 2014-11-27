(define-library (scheme time)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))

  (include-library-declarations "../../interfaces/scheme/time.scm")
  (begin
    (define-native-library lltime (static-library "lltime"))

    (define-r7rs current-second (native-function lltime "lltime_current_second" (-> <native-double>)))
    (define-r7rs current-jiffy (native-function lltime "lltime_current_jiffy" (-> <native-int64>)))
    ; Hardcode this in Scheme
    ; This break encapsulation with the C++ implementation but allows faster code to be generated. In particular,
    ; because the value is known to be non-zero native division instructions can be emitted when it's the denominator.
    (define-r7rs (jiffies-per-second) 1000000000)))
