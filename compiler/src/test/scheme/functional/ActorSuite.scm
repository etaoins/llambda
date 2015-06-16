(define-test "starting an empty actor" (expect-success
  (import (llambda actor))

  (define actor (act (lambda ()
                       (lambda (msg)))))

  (assert-true (mailbox? actor))
  (assert-true (mailbox-open? actor))

  (assert-true (graceful-stop actor))
  (assert-false (mailbox-open? actor))

  ; Second graceful-stop should work
  (assert-true (graceful-stop actor))))

(define-test "(ask) with a timeout" (expect-success
  (import (llambda actor))
  (import (llambda duration))
  (import (llambda error))

  (define actor (act (lambda ()
                       (lambda (msg)))))

  (assert-raises ask-timeout-error?
                 (ask actor #f (milliseconds 20)))))

(define-test "actors with dynamic states" (expect-success
  (import (llambda actor))
  (import (llambda error))
  (import (llambda duration))

  (define test-param (make-parameter 5))

  ; We should take the dynamic state we're created in
  (define my-actor (parameterize ((test-param 10))
                                 (act (lambda ()
                                        (lambda (msg)
                                          (tell (sender) (test-param)))))))

  (assert-equal 10 (ask my-actor 'test (seconds 2)))

  ; We shouldn't be able to clone a parameter procedure. Send to ourselves instead of our actor because our actor
  ; could have already exited
  (assert-raises unclonable-value-error?
    (ask my-actor test-param (seconds 2)))))

(define-test "(self)" (expect-success
  (import (llambda actor))
  (import (llambda error))
  (import (llambda duration))

  (define test-actor
    (act (lambda ()
           ; Haven't received our self message
           (define received-self #f)

           ; Send ourselves a message
           (tell (self) 'self-message)

           (lambda (msg)
             (case msg
               ((self-message)
                (set! received-self #t))

               ((self-is-mailbox?)
                (tell (sender) (mailbox? (self))))

               ((self-mailbox-is-open?)
                (tell (sender) (mailbox-open? (self))))

               ((self-is-self?)
                (tell (sender) (equal? (self) (self))))

               ((received-self?)
                (tell (sender) received-self)))))))

  ; We're not an actor - (self) won't work
  (assert-raises no-actor-error?
                 (self))

  (assert-true (ask test-actor 'received-self? (seconds 2)))
  (assert-true (ask test-actor 'self-is-mailbox? (seconds 2)))
  (assert-true (ask test-actor 'self-mailbox-is-open? (seconds 2)))
  (assert-true (ask test-actor 'self-is-self? (seconds 2)))))

(define-test "(sender) is #!unit when message is sent from non-actor" (expect-success
  (import (llambda actor))
  (import (llambda duration))

  (define test-actor
    (act (lambda ()
           (define saved-sender #f)
           (lambda (msg)
             (cond
               ((equal? msg 'save-sender)
                (set! saved-sender (sender)))
               ((equal? msg 'return-sender)
                (tell (sender) saved-sender)))))))

  (tell test-actor 'save-sender)
  (assert-equal #!unit (ask test-actor 'return-sender (seconds 2)))))

(define-test "actor value cloning" (expect-success
  (import (llambda actor))
  (import (llambda error))
  (import (llambda typed))
  (import (llambda duration))

  (define ping-pong-actor
    (act (lambda ()
           (lambda (msg)
             (tell (sender) msg)))))

  ; Sends a message to our actor and receives it back
  (define (ping-pong val)
    (ask ping-pong-actor val (seconds 2)))

  ; Constants
  (assert-equal 5 (ping-pong 5))
  (assert-equal #t (ping-pong #t))
  (assert-equal #f (ping-pong #f))
  (assert-equal '() (ping-pong '()))
  (assert-equal '(a b c d e) (ping-pong '(a b c d e)))

  ; Numbers
  (assert-equal 5 (ping-pong (+ (typeless-cell 2) 3)))
  (assert-equal .5 (ping-pong (/ (typeless-cell 1) 2)))

  ; Characters
  (define test-char (string-ref (typeless-cell "abc") 1))
  (assert-equal #\b (ping-pong test-char))

  ; Bytevectors
  (define test-bv (make-bytevector 5 0))
  (bytevector-u8-set! test-bv 2 (typeless-cell 1))

  (define cloned-bv (ping-pong test-bv)) ; Clone
  (bytevector-u8-set! cloned-bv 0 255) ; Modify the clone

  (assert-equal #u8(255 0 1 0 0) cloned-bv) ; Clone is modified
  (assert-equal #u8(0 0 1 0 0) test-bv) ; Original is not

  ; Vectors
  (define test-vec (make-vector 5 0))
  (vector-set! test-vec 2 (typeless-cell 'one))

  (define cloned-vec (ping-pong test-vec)) ; Clone
  (vector-set! cloned-vec 0 255) ; Modify the clone

  (assert-equal #(255 0 one 0 0) cloned-vec) ; Clone is modified
  (assert-equal #(0 0 one 0 0) test-vec) ; Original is not

  ; Strings
  (define test-string (make-string 12 #\*))
  (string-set! test-string 0 (typeless-cell #\x2603))

  (define cloned-string (ping-pong test-string)) ; Clone
  (string-set! cloned-string 11 #\!) ; Modify the clone

  (assert-equal "☃**********!" (ping-pong cloned-string)) ; Clone is modified
  (assert-equal "☃***********" (ping-pong test-string)) ; Original is not

  ; Symbols
  (assert-equal '|☃***********| (ping-pong (string->symbol test-string)))

  ; Mailboxes
  (assert-equal ping-pong-actor (ping-pong ping-pong-actor))

  ; Continuations cannot be cloned
  (call/cc (lambda (k)
             (assert-raises unclonable-value-error?
                            (ping-pong k))))

  ; Error objects
  (define orig-error (guard (obj
                              (else
                                obj))
                            (raise-integer-overflow-error "Test error!" 1 2 3)))

  (define cloned-error (ping-pong orig-error))

  (assert-true (integer-overflow-error? cloned-error))
  (assert-equal "Test error!" (error-object-message cloned-error))
  (assert-equal '(1 2 3) (error-object-irritants cloned-error))

  ; Inline records
  (define-record-type <inline-record> (inline-record native-field cell-field) inline-record?
                      ([native-field : <exact-integer>] inline-record-native-field set-inline-record-native-field!)
                      (cell-field inline-record-cell-field))

  (define test-inline-record (inline-record -67 test-vec))

  (define cloned-inline (ping-pong test-inline-record)) ; Clone
  (set-inline-record-native-field! cloned-inline 76) ; Modify the clone

  (assert-true (inline-record? cloned-inline))
  (assert-equal 76 (inline-record-native-field cloned-inline)) ; Clone is modified
  (assert-equal -67 (inline-record-native-field test-inline-record)) ; Original is not
  (assert-equal #(0 0 one 0 0) (inline-record-cell-field cloned-inline))

  ; Out-of-line records
  (define-record-type <ool-record> (ool-record native-field cell-field1 cell-field2) ool-record?
                      ([native-field : <exact-integer>] ool-record-native-field set-ool-record-native-field!)
                      (cell-field1 ool-record-cell-field1)
                      (cell-field2 ool-record-cell-field2))

  (define test-ool-record (ool-record 15 test-vec test-char))
  (define cloned-ool (ping-pong test-ool-record)) ; Clone
  (set-ool-record-native-field! cloned-ool -51) ; Modify the clone

  (assert-true (ool-record? cloned-ool))
  (assert-equal -51 (ool-record-native-field cloned-ool)) ; Clone is modified
  (assert-equal 15 (ool-record-native-field test-ool-record)) ; Clone is modified
  (assert-equal #(0 0 one 0 0) (ool-record-cell-field1 cloned-ool))
  (assert-equal #\b (ool-record-cell-field2 cloned-ool))

  ; Pairs
  (cond-expand
    ((not immutable-pairs)

     (let ((test-pair (cons 1 2)))
       (set-car! test-pair 3)
       (let ((cloned-pair (ping-pong test-pair))) ; Clone
         (set-cdr! cloned-pair 4) ; Modify the clone
         (assert-equal cloned-pair '(3 . 4)) ; Clone is modified
         (assert-equal test-pair '(3 . 2)))))) ; Original is not

  ; stdin, stdout and stderr can be cloned
  (assert-equal (current-input-port) (ping-pong (current-input-port)))
  (assert-equal (current-output-port) (ping-pong (current-output-port)))
  (assert-equal (current-error-port) (ping-pong (current-error-port)))

  ; Cloning preserves (eqv?)
  (define same-elem-vec (ping-pong (vector test-vec test-vec test-vec)))

  (assert-true (eqv? (vector-ref same-elem-vec 0) (vector-ref same-elem-vec 1)))
  (assert-true (eqv? (vector-ref same-elem-vec 1) (vector-ref same-elem-vec 2)))))

(define-test "concurrent actor startup and shutdown" (expect-success
  (import (llambda typed))
  (import (llambda actor))
  (import (llambda duration))

  (define-record-type <start-message> (start-message children) start-message?
                      ([children : <exact-integer>] start-message-children))

  (define-record-type <result-message> (result-message count) result-message?
                      ([count : <exact-integer>] result-message-count))

  (: replicating-actor (-> (-> <any> <unit>)))
  (define (replicating-actor)
    (define waiting-children : <exact-integer> 0)
    (define started-mailbox #!unit)
    (define reply-sum : <exact-integer> 0)

    (lambda (msg)
      (cond
        ((start-message? msg)
         (define count (start-message-children msg))

         (if (= count 0)
           ; All done
           (begin
             (tell (sender) (result-message 1))
             (stop (self)))
           (begin
             ; Set our state
             (set! started-mailbox (sender))
             (set! waiting-children count)

             ; Start some children
             (for-each (lambda (_)
                         ; Have this actor start one less child than we did
                         (tell (act replicating-actor) (start-message (- count 1)))
                         ) (make-list count)))))

        ((result-message? msg)
         ; Add the reply to our total sum
         (set! reply-sum (+ (result-message-count msg) reply-sum))
         (set! waiting-children (- waiting-children 1))

         (if (= waiting-children 0)
           (begin
             (tell started-mailbox (result-message reply-sum))
             (stop (self))))))))

  (define root-actor (act replicating-actor))
  (define result (ask root-actor (start-message 6) (seconds 20)))

  (assert-equal 720 (result-message-count result))))

(define-test "captured continuations cannot be used across messages" (expect-success
  (import (llambda actor))
  (import (llambda error))
  (import (llambda duration))

  (define actor
    (act (lambda ()
           (define captured-cont #!unit)
           (lambda (msg)
             (cond
               ((equal? msg 'capture)
                (call/cc (lambda (k)
                           (set! captured-cont k))))
               ((equal? msg 'invoke)
                (guard (obj
                         (else
                           (tell (sender) obj)))
                       (captured-cont))
                (tell (sender) 'no-except)))))))

  (tell actor 'capture)
  (assert-true (expired-escape-procedure-error? (ask actor 'invoke (seconds 2))))))

(define-test "procedures capturing ports can be used inside actors" (expect-success
  (import (scheme write))
  (import (llambda actor))
  (import (llambda duration))

  (define actor
    (act (lambda ()
           (define output-string (open-output-string))

           (lambda (msg)
             (cond
               ((equal? 'return-result msg)
                (tell (sender) (get-output-string output-string)))
               ((equal? 'append (car msg))
                ; codegen captures (current-output-port) but never uses it
                (display (cdr msg) output-string)))))))

  (tell actor '(append . "abc"))
  (tell actor '(append . "123"))
  (tell actor '(append . "!"))

  (assert-equal "abc123!" (ask actor 'return-result (seconds 2)))))

(define-test "top-level failure action is restart" (expect-success
  (import (llambda actor))
  (import (llambda duration))

  (define actor
    (act (lambda ()
           (define counter 0)

           (lambda (msg)
             (cond
               ((equal? 'increment msg)
                (set! counter (+ counter 1)))
               ((equal? 'query msg)
                (tell (sender) counter))
               ((equal? 'fail msg)
                (raise "FAILURE")))))))

  (tell actor 'increment)
  (tell actor 'increment)
  (tell actor 'increment)

  (assert-equal 3 (ask actor 'query (seconds 2)))

  (tell actor 'fail)
  (tell actor 'increment)

  ; This should have reset our state
  (assert-equal 1 (ask actor 'query (seconds 2)))))

(define-test "restart failure action" (expect-success
  (import (llambda actor))
  (import (llambda duration))

  (act (lambda ()
    (define actor
      (act (lambda ()
             (define counter 0)

             (lambda (msg)
               (cond
                 ((equal? 'increment msg)
                  (set! counter (+ counter 1)))
                 ((equal? 'query msg)
                  (tell (sender) counter))
                 ((equal? 'fail msg)
                  (raise "FAILURE")))))))

    ; Restart our children
    (set-supervisor-strategy (lambda (err)
                               (assert-equal "FAILURE" err)
                               'restart))

    (tell actor 'increment)
    (tell actor 'increment)
    (tell actor 'increment)

    (assert-equal 3 (ask actor 'query (seconds 2)))

    (tell actor 'fail)
    (tell actor 'increment)

    (tell actor 'query)

    (lambda (msg)
      ; This should have reset our state
      (assert-equal 1 msg))))))

(define-test "resume failure action" (expect-success
  (import (llambda actor))
  (import (llambda duration))

  (act (lambda ()
    (define actor
      (act (lambda ()
             (define counter 0)

             (lambda (msg)
               (cond
                 ((equal? 'increment msg)
                  (set! counter (+ counter 1)))
                 ((equal? 'query msg)
                  (tell (sender) counter))
                 ((equal? 'fail msg)
                  (raise "FAILURE")))))))

    ; Resume our children
    (set-supervisor-strategy (lambda (err)
                               (assert-equal "FAILURE" err)
                               'resume))

    (tell actor 'increment)
    (tell actor 'increment)
    (tell actor 'increment)

    (assert-equal 3 (ask actor 'query (seconds 2)))

    (tell actor 'fail)
    (tell actor 'increment)

    (tell actor 'query)

    (lambda (msg)
      ; This should keep our state
      (assert-equal 4 msg))))))

(define-test "stop failure action" (expect-success
  (import (llambda actor))
  (import (llambda duration))
  (import (llambda error))

  (act (lambda ()
    (define actor
      (act (lambda ()
             (define counter 0)

             (lambda (msg)
               (cond
                 ((equal? 'increment msg)
                  (set! counter (+ counter 1)))
                 ((equal? 'query msg)
                  (tell (sender) counter))
                 ((equal? 'fail msg)
                  (raise "FAILURE")))))))

    ; Restart our children
    (set-supervisor-strategy (lambda (err)
                               (assert-equal "FAILURE" err)
                               'stop))

    (tell actor 'increment)
    (tell actor 'increment)
    (tell actor 'increment)

    (assert-equal 3 (ask actor 'query (seconds 2)))

    (tell actor 'fail)
    (tell actor 'increment)
    (tell actor 'query)

    (schedule-once (milliseconds 250) (self) 'timed-out)

    (lambda (msg)
      (assert-equal 'timed-out msg))))))

(define-test "poison pills" (expect-success
  (import (llambda actor))
  (import (llambda error))
  (import (llambda duration))

  (define poison-pill (poison-pill-object))
  (assert-true (poison-pill-object? poison-pill))
  (assert-false (poison-pill-object? #f))

  (define child-actor-closure
    (lambda ()
      (define counter 0)

      (lambda (msg)
        (cond
          ((equal? 'shouldnt-receive msg)
           (assert-true #f))
          ((equal? 'increment msg)
           (set! counter (+ counter 1)))
          ((equal? 'query msg)
           (tell (sender) counter))))))

  (define parent-actor-closure
    (lambda ()
      (define child-actor (act child-actor-closure))
      (define reply-mailbox #f)
      (define reply #f)

      (lambda (msg)
        (cond
          ((equal? 'go msg)
           ; Save the sender
           (set! reply-mailbox (sender))

           (tell child-actor 'increment)
           (tell child-actor 'increment)
           (tell child-actor 'increment)
           (tell child-actor 'query)
           (tell child-actor poison-pill)
           (tell child-actor 'shouldnt-receive))

          (else
            ; Relay this message to the outer scope
            (tell reply-mailbox msg))))))

  (define parent-actor (act parent-actor-closure))
  (assert-equal 3 (ask parent-actor 'go (seconds 2)))))

(define-test "(become)" (expect-success
  (import (llambda actor))
  (import (llambda duration))

  ; This is stolen from the Akka documentation
  (define actor
    (act (lambda ()
           (define (angry msg)
             (cond
               ((equal? "foo" msg)
                (tell (sender) "I'm already angry?"))
               ((equal? "bar" msg)
                (become happy))))

           (define (happy msg)
             (cond
               ((equal? "bar" msg)
                (tell (sender) "I'm already happy :-)"))
               ((equal? "foo" msg)
                (become happy))))

           (define (initial msg)
             (cond
               ((equal? "foo" msg)
                (become angry))
               ((equal? "bar" msg)
                (become happy))))

           initial)))

  (tell actor "foo")
  (assert-equal "I'm already angry?" (ask actor "foo" (seconds 2)))

  (tell actor "bar")
  (assert-equal "I'm already happy :-)" (ask actor "bar" (seconds 2)))))

(define-test "(forward)" (expect-success
  (import (llambda actor))
  (import (llambda duration))

  (define double-incrementer
    (act (lambda ()
           (define child-actor
             (act (lambda ()
                    (define counter 0)

                    (lambda (msg)
                      (cond
                        ((equal? 'increment msg)
                         (set! counter (+ counter 1)))
                        ((equal? 'query msg)
                         (tell (sender) counter))
                        ((equal? 'fail msg)
                         (raise "FAILURE")))))))

           (set-supervisor-strategy
             (lambda (err)
               'resume))

           (lambda (msg)
             (cond
               ((equal? msg 'increment)
                ; Forward this message twice
                (forward child-actor 'increment)
                (forward child-actor 'increment))
               (else
                 (forward child-actor msg)))))))

  (tell double-incrementer 'increment)
  (tell double-incrementer 'increment)
  (tell double-incrementer 'increment)

  (assert-equal 6 (ask double-incrementer 'query (seconds 2)))

  ; The double-incrementer should use its resume stategy here
  (tell double-incrementer 'fail)

  (tell double-incrementer 'increment)
  (assert-equal 8 (ask double-incrementer 'query (seconds 2)))))

(define-test "failure in supervisor strategy escalates to supervisor's supervisor" (expect-success
  (import (llambda actor))
  (import (llambda duration))

  (define supervisor-supervisor
    (act (lambda ()
           (define outer-mailbox #f)

           (define supervisor
             (act (lambda ()
                    (define child
                      (act (lambda ()
                             (lambda (msg)
                               (when (equal? msg 'fail)
                                 (raise "FIRST ERROR"))))))

                    (set-supervisor-strategy (lambda (err)
                                               (assert-equal "FIRST ERROR" err)
                                               (raise "SECOND ERROR")
                                               'stop))
                    (lambda (msg)
                      (when (equal? msg 'go)
                        ; Get our child to fail
                        (tell child 'fail))))))

           (set-supervisor-strategy (lambda (err)
                                      (tell outer-mailbox err)
                                      'stop))

           (lambda (msg)
             (when (equal? msg 'go)
               ; Save our sender
               (set! outer-mailbox (sender))
               ; Start our child
               (tell supervisor 'go))))))

  (assert-equal "SECOND ERROR" (ask supervisor-supervisor 'go (seconds 2)))))

(define-test "escalate failure action" (expect-success
  (import (llambda actor))
  (import (llambda duration))

  (define supervisor-supervisor
    (act (lambda ()
           (define outer-mailbox #f)

           (define supervisor
             (act (lambda ()
                    (define child
                      (act (lambda ()
                             (lambda (msg)
                               (when (equal? msg 'fail)
                                 (raise "FIRST ERROR"))))))

                    (set-supervisor-strategy (lambda (err)
                                               'escalate))

                    (lambda (msg)
                      (when (equal? msg 'go)
                        ; Get our child to fail
                        (tell child 'fail))))))

           (set-supervisor-strategy (lambda (err)
                                      (tell outer-mailbox err)
                                      'stop))

           (lambda (msg)
             (when (equal? msg 'go)
               ; Save our sender
               (set! outer-mailbox (sender))
               ; Start our child
               (tell supervisor 'go))))))

  (assert-equal "FIRST ERROR" (ask supervisor-supervisor 'go (seconds 2)))))

(define-test "(schedule-once)" (expect-success
  (import (llambda actor))
  (import (llambda duration))

  (define test-actor (act
                       (lambda ()
                         (define received-messages '())
                         (define result-sender #f)
                         (define target-length 6)

                         (schedule-once (milliseconds 50) (self) 5)
                         (schedule-once (milliseconds 10) (self) 1)
                         (schedule-once (milliseconds 30) (self) 3)
                         (schedule-once (milliseconds 20) (self) 2)
                         (schedule-once (milliseconds 40) (self) 4)
                         (schedule-once (milliseconds 0) (self) 0)

                         (lambda (msg)
                           (if (equal? msg 'get-result)
                             (if (= (length received-messages) target-length)
                               (tell (sender) received-messages)
                               (set! result-sender (sender)))
                             (begin
                               (set! received-messages (append received-messages (list msg)))
                               (if (and result-sender (= (length received-messages) target-length))
                                 (tell result-sender received-messages))))))))

  (define result (ask test-actor 'get-result (milliseconds 250)))
  (assert-equal '(0 1 2 3 4 5) result)))
