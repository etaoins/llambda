(define-library (llambda actor)
  (import (scheme base))
  (import (llambda nfi))
  (import (llambda typed))

  (export act tell ask self sender stop mailbox? mailbox-open? <mailbox> <behaviour>)

  (begin
    (define-native-library llactor (static-library "ll_llambda_actor"))

    (define-type <behaviour> (-> <any> <unit>))

    (define act (world-function llactor "llactor_act" (-> (-> <behaviour>) <mailbox>)))
    (define tell (world-function llactor "llactor_tell" (-> <mailbox> <any> <unit>)))
    (define ask (world-function llactor "llactor_ask" (-> <mailbox> <any> <any>)))
    (define self (world-function llactor "llactor_self" (-> <mailbox>)))
    (define sender (world-function llactor "llactor_sender" (-> (U <unit> <mailbox>))))
    (define stop (native-function llactor "llactor_stop" (-> <mailbox> <unit>)))

    (define-predicate mailbox? <mailbox>)
    (define mailbox-open? (world-function llactor "llactor_mailbox_is_open" (-> <mailbox> <native-bool>)))))
