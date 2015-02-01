(define-library (llambda actor)
  (import (scheme base))
  (import (llambda nfi))
  (import (llambda typed))

  (export act ! receive sender self mailbox? mailbox-open? <mailbox>)

  (begin
    (define-native-library llactor (static-library "ll_llambda_actor"))

    (define act (world-function llactor "llactor_act" (-> (-> <unit>) <mailbox>)))
    (define ! (world-function llactor "llactor_send" (-> <mailbox> <any> <unit>)))
    (define receive (world-function llactor "llactor_receive" (-> <any>)))
    (define self (world-function llactor "llactor_self" (-> <mailbox>)))
    (define sender (world-function llactor "llactor_sender" (-> (U <unit> <mailbox>))))
    (define-predicate mailbox? <mailbox>)
    (define mailbox-open? (world-function llactor "llactor_mailbox_is_open" (-> <mailbox> <native-bool>)))))
