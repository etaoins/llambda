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

  ; Constants
  (assert-equal 5 (ping-pong 5))
  (assert-equal #t (ping-pong #t))
  (assert-equal #f (ping-pong #f))
  (assert-equal '() (ping-pong '()))

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

  (! ping-pong-actor 'exit)))
