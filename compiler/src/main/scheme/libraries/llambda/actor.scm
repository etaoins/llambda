(define-library (llambda actor)
  (import (scheme base))
  (import (llambda nfi))
  (import (llambda typed))

  (export act tell ask self sender stop graceful-stop mailbox? mailbox-open? child-failure-action
          set-child-failure-action resume-failure-action restart-failure-action stop-failure-action <mailbox>
          <behaviour> <failure-action>)

  (begin
    (define-native-library llactor (static-library "ll_llambda_actor"))

    (define-record-type <failure-action> (failure-action enum-value) failure-action?
                        ([enum-value : <exact-integer>] failure-action-enum-value))

    (define-type <behaviour> (-> <any> <unit>))

    (define act (world-function llactor "llactor_act" (-> (-> <behaviour>) <mailbox>)))
    (define tell (world-function llactor "llactor_tell" (-> <mailbox> <any> <unit>)))
    (define ask (world-function llactor "llactor_ask" (-> <mailbox> <any> <any>)))
    (define self (world-function llactor "llactor_self" (-> <mailbox>)))
    (define sender (world-function llactor "llactor_sender" (-> (U <unit> <mailbox>))))
    (define stop (native-function llactor "llactor_stop" (-> <mailbox> <unit>)))
    (define graceful-stop (native-function llactor "llactor_graceful_stop" (-> <mailbox> <native-bool>)))

    ; These match the values in runtime/actor/FailureAction.h
    (define resume-failure-action (failure-action 0))
    (define restart-failure-action (failure-action 1))
    (define stop-failure-action (failure-action 2))

    (define native-set-child-failure-action (world-function llactor "llactor_set_child_failure_action" (-> <native-uint32> <unit>)))

    (: set-child-failure-action (-> <failure-action> <unit>))
    (define (set-child-failure-action action)
      (native-set-child-failure-action (failure-action-enum-value action)))

    (define native-child-failure-action (world-function llactor "llactor_child_failure_action" (-> <native-uint32>)))

    (: child-failure-action (-> <failure-action>))
    (define (child-failure-action)
      (define enum-value (native-child-failure-action))

      (cond
        ((equal? enum-value (failure-action-enum-value resume-failure-action))
         resume-failure-action)
        ((equal? enum-value (failure-action-enum-value restart-failure-action))
         restart-failure-action)
        (else
          stop-failure-action)))

    (define-predicate mailbox? <mailbox>)
    (define mailbox-open? (world-function llactor "llactor_mailbox_is_open" (-> <mailbox> <native-bool>)))))
