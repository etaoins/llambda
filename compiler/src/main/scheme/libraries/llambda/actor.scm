(define-library (llambda actor)
  (import (scheme base))
  (import (llambda nfi))
  (import (llambda typed))
  (import (llambda duration))

  (export act tell ask self sender stop graceful-stop mailbox? mailbox-open? poison-pill-object poison-pill-object?
          become set-supervisor-strategy <mailbox> <behaviour> <failure-action> <supervisor-strategy>)

  (begin
    (define-native-library llactor (static-library "ll_llambda_actor"))

    (define-type <failure-action> (U 'resume 'restart 'stop))
    (define-type <supervisor-strategy> (-> <any> <failure-action>))

    ; These match the values in runtime/actor/FailureAction.h
    (define resume-enum-value 0)
    (define restart-enum-value 1)
    (define stop-enum-value 2)

    (define-type <behaviour> (-> <any> <unit>))

    (define act (world-function llactor "llactor_act" (-> (-> <behaviour>) <mailbox>)))
    (define tell (world-function llactor "llactor_tell" (-> <mailbox> <any> <unit>)))
    (define ask (world-function llactor "llactor_ask" (-> <mailbox> <any> <native-int64> <any>)))
    (define self (world-function llactor "llactor_self" (-> <mailbox>)))
    (define sender (world-function llactor "llactor_sender" (-> (U <unit> <mailbox>))))
    (define stop (native-function llactor "llactor_stop" (-> <mailbox> <unit>)))
    (define graceful-stop (native-function llactor "llactor_graceful_stop" (-> <mailbox> <native-bool>)))

    (define-predicate mailbox? <mailbox>)
    (define mailbox-open? (world-function llactor "llactor_mailbox_is_open" (-> <mailbox> <native-bool>)))

    (define poison-pill-object (native-function llactor "llactor_poison_pill_object" (-> <any>)))
    (define poison-pill-object? (native-function llactor "llactor_is_poison_pill_object" (-> <any> <native-bool>)))

    (define become (world-function llactor "llactor_become" (-> <behaviour> <unit>)))

    (define set-supervisor-strategy (world-function llactor "llactor_set_supervisor_strategy" (-> <supervisor-strategy> <unit>)))))
