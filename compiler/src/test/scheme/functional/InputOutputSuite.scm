(define-test "eof-object" (expect-success
  (import (llambda typed))
  (assert-true  (eof-object? (eof-object)))
  (assert-false (eof-object? ""))
  (assert-false (eof-object? #\x00))
  (assert-false (eof-object? #u8()))
  (ann (eof-object) <eof-object>)))

(define-test "written strings can be read" (expect-output ("Hello!")
  (import (scheme write))
  (write "Hello!")))

(define-test "writing consecutive numbers does not introduce whitespace" (expect-output (123)
  (import (scheme write))
  (write 1)
  (write 2)
  (write 3)))

(define-test "writing consecutive numbers with newlines introduces whitespace" (expect-output (1 2 3)
  (import (scheme write))
  (write 1)
  (newline)
  (write 2)
  (newline)
  (write 3)))

(define-test "(display) does not quote strings or characters" (expect-output ("Hello!")
  (import (scheme write))
  (display #\")
  (display "Hello!")
  (display #\")))

(define-test "(display) does not quote symbols" (expect-output ("Hello!")
  (import (scheme write))
  (display #\")
  (display '|Hello!|)
  (display #\")))

(define-test "(read-u8), (peek-u8)" (expect-success
  (define input-string (open-input-string "Hell☃!"))

  (assert-equal #x48 (read-u8 input-string))
  (assert-equal #x65 (read-u8 input-string))

  (assert-equal #x6c (peek-u8 input-string))
  (assert-equal #x6c (peek-u8 input-string))

  (assert-equal #x6c (read-u8 input-string))
  (assert-equal #x6c (read-u8 input-string))

  (parameterize
    ((current-input-port input-string))
    (assert-equal #xe2 (read-u8))
    (assert-equal #x98 (read-u8))
    (assert-equal #x83 (read-u8))
    (assert-equal #x21 (read-u8)))

  (assert-true (eof-object? (peek-u8 input-string)))
  (assert-true (eof-object? (read-u8 input-string)))))
