(define-test "starting an empty actor" (expect-success
  (import (llambda actor))

  (define actor (act (lambda ())))

  (assert-true (mailbox? actor))))

(define-test "actor exiting the process" (expect-exit-value 5
  (import (llambda actor))
  (import (scheme process-context))

  (act (lambda ()
         (exit 5)))

  ; Block the parent actor
  (read-line)))

(define-test "(self)" (expect-success
  (import (llambda actor))

  (assert-true (mailbox? (self)))
  (assert-true (mailbox-open? (self)))

  (! (self) 'message)
  (assert-equal 'message (receive))))

(define-test "actor value cloning" (expect-success
  (import (llambda actor))

  (define ping-pong-actor
    (act (lambda ()
           (let loop ()
             (define msg (receive))

             (unless (equal? msg 'exit)
               (! (sender) msg)
               (loop))))))

  ; Sends a message to our actor and receives it back
  (define (ping-pong val)
    (! ping-pong-actor val)
    (receive))

  (assert-equal 5 (ping-pong 5))
  (assert-equal #t (ping-pong #t))
  (assert-equal #f (ping-pong #f))
  (assert-equal '() (ping-pong '()))

  (! ping-pong-actor 'exit)))
