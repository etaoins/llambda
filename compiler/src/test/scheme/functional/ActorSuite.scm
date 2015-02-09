(define-test "starting an empty actor" (expect-success
  (import (llambda actor))

  (define actor (act (lambda ()
                       (lambda (msg)))))

  (assert-true (mailbox? actor))))

(define-test "actors with dynamic states" (expect-success
  (import (llambda actor))
  (import (llambda error))

  (define test-param (make-parameter 5))

  ; We should take the dynamic state we're created in
  (define my-actor (parameterize ((test-param 10))
                                 (act (lambda ()
                                        (lambda (msg)
                                          (tell (sender) (test-param)))))))

  (assert-equal 10 (ask my-actor 'test))

  ; We shouldn't be able to clone a parameter procedure. Send to ourselves instead of our actor because our actor
  ; could have already exited
  (assert-raises unclonable-value-error?
    (ask my-actor test-param))))

(define-test "(self)" (expect-success
  (import (llambda actor))
  (import (llambda error))

  (define test-actor
    (act (lambda ()
           ; Haven't received our self message
           (define received-self #f)

           (lambda (msg)
             (case msg
               ((send-self-message)
                ; Send ourselves a test message
                (tell (self) 'self-message)
                (tell (sender) 'okay))

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

  ; Have the actor send itself a message
  (assert-equal 'okay (ask test-actor 'send-self-message))

  (assert-true (ask test-actor 'received-self?))
  (assert-true (ask test-actor 'self-is-mailbox?))
  (assert-true (ask test-actor 'self-mailbox-is-open?))
  (assert-true (ask test-actor 'self-is-self?))))

(define-test "(sender) is #!unit when message is sent from non-actor" (expect-success
  (import (llambda actor))

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
  (assert-equal #!unit (ask test-actor 'return-sender))))

(define-test "actor value cloning" (expect-success
  (import (llambda actor))
  (import (llambda error))
  (import (llambda typed))

  (define ping-pong-actor
    (act (lambda ()
           (lambda (msg)
             (tell (sender) msg)))))

  ; Sends a message to our actor and receives it back
  (define (ping-pong val)
    (ask ping-pong-actor val))

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

  (define (replicating-actor)
    (define started-children : <exact-integer> 0)
    (define started-mailbox #!unit)

    (define reply-count : <exact-integer> 0)
    (define reply-sum : <exact-integer> 0)

    (lambda (msg)
      (cond
        ((equal? (car msg) 'start)
         (define count (cdr msg))

         ; Set our state
         (set! started-mailbox (sender))
         (set! started-children count)

         (if (= count 0)
           ; All done
           (begin
             (tell (sender) (cons 'result 1))
             (stop (self)))
           ; Start some children
           (map (lambda (_)
                  ; Have this actor start one less child than we did
                  (tell (act replicating-actor) (cons 'start (- count 1)))
                  ) (make-list count))))

        ((equal? (car msg) 'result)
         ; Add the reply to our total sum
         (set! reply-sum (+ (cdr msg) reply-sum))
         (set! reply-count (+ reply-count 1))

         (if (= reply-count started-children)
           (begin
             (tell started-mailbox (cons 'result reply-sum))
             (stop (self))))))))

  (define root-actor (act replicating-actor))
  (assert-equal (cons 'result 720) (ask root-actor (cons 'start 6)))))

(define-test "captured continuations cannot be used across messages" (expect-success
  (import (llambda actor))
  (import (llambda error))

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
  (assert-true (expired-escape-procedure-error? (ask actor 'invoke)))))

(define-test "procedures capturing ports can be used inside actors" (expect-success
  (import (scheme write))
  (import (llambda actor))

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

  (assert-equal "abc123!" (ask actor 'return-result))))
