(define-library (llambda actor)
  (import (scheme base))
  (import (llambda typed))
  (import (llambda nfi))

  (export start-actor)

  (begin
    (define-native-library llactor (static-library "ll_llambda_actor"))

    (define start-actor (world-function llactor "llactor_start_actor" (-> (-> <unit>) <unit>)))))
