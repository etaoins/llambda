(define-library (llambda random)
  (import (llambda internal primitives))
  (import (llambda nfi))

  (export random-integer random-real)
  (begin
    (define-native-library llrandom (static-library "ll_llambda_random"))
    (define random-integer (world-function llrandom "llrandom_random_integer" (-> <native-int64> <native-int64>)))
    (define random-real (native-function llrandom "llrandom_random_real" (-> <native-double>)))))
