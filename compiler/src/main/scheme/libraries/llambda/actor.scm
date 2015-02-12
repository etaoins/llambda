(define-library (llambda actor)
  (import (scheme base))
  (import (llambda nfi))
  (import (llambda typed))

  (export act tell ask self sender stop graceful-stop mailbox? mailbox-open? child-failure-action
          set-child-failure-action <mailbox> <behaviour> <failure-action>)

  (begin
    (define-native-library llactor (static-library "ll_llambda_actor"))

    (define-type <failure-action> (U 'resume 'restart 'stop))

    ; These match the values in runtime/actor/FailureAction.h
    (define resume-enum-value 0)
    (define restart-enum-value 1)
    (define stop-enum-value 2)

    (define-type <behaviour> (-> <any> <unit>))

    (define act (world-function llactor "llactor_act" (-> (-> <behaviour>) <mailbox>)))
    (define tell (world-function llactor "llactor_tell" (-> <mailbox> <any> <unit>)))
    (define ask (world-function llactor "llactor_ask" (-> <mailbox> <any> <any>)))
    (define self (world-function llactor "llactor_self" (-> <mailbox>)))
    (define sender (world-function llactor "llactor_sender" (-> (U <unit> <mailbox>))))
    (define stop (native-function llactor "llactor_stop" (-> <mailbox> <unit>)))
    (define graceful-stop (native-function llactor "llactor_graceful_stop" (-> <mailbox> <native-bool>)))

    (define native-set-child-failure-action (world-function llactor "llactor_set_child_failure_action" (-> <native-uint32> <unit>)))

    (: set-child-failure-action (-> <failure-action> <unit>))
    (define (set-child-failure-action action)
      (cond
        ((equal? action 'resume)
         (native-set-child-failure-action resume-enum-value))
        ((equal? action 'restart)
         (native-set-child-failure-action restart-enum-value))
        ((equal? action 'stop)
         (native-set-child-failure-action stop-enum-value))))

    (define native-child-failure-action (world-function llactor "llactor_child_failure_action" (-> <native-uint32>)))

    (: child-failure-action (-> <failure-action>))
    (define (child-failure-action)
      (define enum-value (native-child-failure-action))

      (cond
        ((equal? enum-value resume-enum-value) 'resume)
        ((equal? enum-value restart-enum-value) 'restart)
        (else 'stop)))

    (define-predicate mailbox? <mailbox>)
    (define mailbox-open? (world-function llactor "llactor_mailbox_is_open" (-> <mailbox> <native-bool>)))))
