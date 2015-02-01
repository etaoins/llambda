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

(define-test "actors with dynamic states" (expect-success
  (import (llambda actor))
  (import (llambda error))

  (define test-param (make-parameter 5))

  ; We should take the dynamic state we're created in
  (define my-actor (parameterize ((test-param 10))
                                 (act (lambda ()
                                        (receive)
                                        (! (sender) (test-param))))))

  (! my-actor 'test)
  (assert-equal 10 (receive))

  ; We shouldn't be able to clone a parameter procedure. Send to ourselves instead of our actor because our actor
  ; could have already exited
  (assert-raises unclonable-value-error?
    (! (self) test-param))))

(define-test "(self)" (expect-success
  (import (llambda actor))

  (assert-true (mailbox? (self)))
  (assert-true (mailbox-open? (self)))

  (assert-equal (self) (self))

  (! (self) 'message)
  (assert-equal 'message (receive))))

(define-test "actor value cloning" (expect-success
  (import (llambda actor))
  (import (llambda error))
  (import (llambda typed))

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
  (assert-equal #u8(0 0 1 0 0) (ping-pong test-bv))

  ; Vectors
  (define test-vec (make-vector 5 0))
  (vector-set! test-vec 2 (typeless-cell 'one))
  (assert-equal #(0 0 one 0 0) (ping-pong test-vec))

  ; Strings
  (define test-string (make-string 12 #\*))
	(string-set! test-string 0 (typeless-cell #\x2603))
	(assert-equal "☃***********" (ping-pong test-string))

  ; Symbols
	(assert-equal '|☃***********| (ping-pong (string->symbol test-string)))

  ; Mailboxes
  (define copied-mailbox (ping-pong (self)))

  (assert-equal copied-mailbox (self))

  (! copied-mailbox 'hello-self)
  (assert-equal 'hello-self (receive))

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
                      ([native-field : <exact-integer>] inline-record-native-field)
                      (cell-field inline-record-cell-field))

  (define test-inline-record (inline-record -67 test-vec))
  (define cloned-inline (ping-pong test-inline-record))

  (assert-true (inline-record? cloned-inline))
  (assert-equal -67 (inline-record-native-field cloned-inline))
  (assert-equal #(0 0 one 0 0) (inline-record-cell-field cloned-inline))

  ; Out-of-line records
  (define-record-type <ool-record> (ool-record native-field cell-field1 cell-field2) ool-record?
                      ([native-field : <exact-integer>] ool-record-native-field)
                      (cell-field1 ool-record-cell-field1)
                      (cell-field2 ool-record-cell-field2))

  (define test-ool-record (ool-record 15 test-vec test-char))
  (define cloned-ool (ping-pong test-ool-record))

  (assert-true (ool-record? cloned-ool))
  (assert-equal 15 (ool-record-native-field cloned-ool))
  (assert-equal #(0 0 one 0 0) (ool-record-cell-field1 cloned-ool))
  (assert-equal #\b (ool-record-cell-field2 cloned-ool))

  ; Pairs
  (cond-expand
    ((not immutable-pairs)

     (let ((test-pair (cons 1 2)))
       (set-car! test-pair 3)
       (assert-equal '(3 . 2) (ping-pong test-pair)))))

  (! ping-pong-actor 'exit)))
