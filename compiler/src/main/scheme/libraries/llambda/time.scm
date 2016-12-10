(define-library (llambda time)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-stdlib-procedure define-stdlib)))

  (export current-unix-time current-jiffy jiffies-per-second)

  (begin
    (define-native-library lltime (static-library "ll_llambda_time"))

    (define-stdlib current-unix-time (native-function lltime "lltime_current_unix_time" (-> <native-double>)))
    (define-stdlib current-jiffy (native-function lltime "lltime_current_jiffy" (-> <native-int64>)))
    ; Hardcode this in Scheme
    ; This breaks encapsulation with the C++ implementation but allows faster code to be generated. In particular,
    ; because the value is known to be non-zero native division instructions can be emitted when it's the denominator.
    (define-stdlib (jiffies-per-second) 1000000000)))
