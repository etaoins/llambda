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

    ; Peeking the last character should still allow it to be read
    (assert-equal #x21 (peek-u8))
    (assert-equal #x21 (read-u8)))

  (assert-true (eof-object? (peek-u8 input-string)))
  (assert-true (eof-object? (read-u8 input-string)))))

(define-test "(write-u8)" (expect-success
  (define output-string (open-output-string))

  (write-u8 #x48 output-string)
  (write-u8 #x65 output-string)
  (write-u8 #x6c output-string)
  (write-u8 #x6c output-string)

  (parameterize
    ((current-output-port output-string))
    (write-u8 #xe2 output-string)
    (write-u8 #x98 output-string)
    (write-u8 #x83 output-string)
    (write-u8 #x21 output-string))

  (assert-equal "Hell☃!" (get-output-string output-string))))

(define-test "(read-char), (peek-char)" (expect-success
  (define empty-port (open-input-string ""))
  (assert-true (eof-object? (peek-char empty-port)))
  (assert-true (eof-object? (read-char empty-port)))

  (define ascii-port (open-input-string "Hello!"))
  (parameterize ((current-input-port ascii-port))
    (assert-equal #\H (read-char))
    (assert-equal #\e (read-char))
    (assert-equal #\l (read-char))
    (assert-equal #\l (read-char))

    (assert-equal #\o (peek-char))
    (assert-equal #\o (peek-char))
    (assert-equal #\o (read-char))

    (assert-equal #\! (peek-char))
    (assert-equal #\! (read-char)))

  (assert-true (eof-object? (read-char ascii-port)))

  (define valid-utf8-port (open-input-string "Hell☃!"))
  (assert-equal #\H (read-char valid-utf8-port))
  (assert-equal #\e (read-char valid-utf8-port))
  (assert-equal #\l (read-char valid-utf8-port))
  (assert-equal #\l (read-char valid-utf8-port))

  (assert-equal #\☃ (peek-char valid-utf8-port))
  (assert-equal #\☃ (read-char valid-utf8-port))

  (assert-equal #\! (read-char valid-utf8-port))

  (define truncated-utf8-port (open-input-bytevector #u8(#x31 #xe2 #x98)))
  (assert-equal #\1 (read-char truncated-utf8-port))
  ; This will EOF due to the port ending mid-byte sequence
  (assert-true (eof-object? (peek-char truncated-utf8-port)))
  ; We should be able to read the remaining bytes using (read-utf8)
  (assert-equal #xe2 (read-u8 truncated-utf8-port))
  (assert-equal #x98 (read-u8 truncated-utf8-port))

  (define invalid-utf8-header-port (open-input-bytevector #u8(#x31 #xff #x33)))
  (assert-equal #\1 (read-char invalid-utf8-header-port))
  (assert-raises (peek-char invalid-utf8-header-port))     ; Invalid header byte
  (assert-raises (read-char invalid-utf8-header-port))
  (assert-equal #\3 (read-char invalid-utf8-header-port))  ; Should recover at next character

  (define invalid-utf8-overlong-port (open-input-bytevector #u8(#x31 #xe0 #x80 #xaf #x33)))
  (assert-equal #\1 (read-char invalid-utf8-overlong-port))
  (assert-raises (peek-char invalid-utf8-overlong-port))     ; Overlong encoding
  (assert-raises (read-char invalid-utf8-overlong-port))
  (assert-equal #\3 (read-char invalid-utf8-overlong-port))  ; Should recover at next character

  (define invalid-utf8-no-continue-port (open-input-bytevector #u8(#x31 #xe2 #x98 #x33)))
  (assert-equal #\1 (read-char invalid-utf8-no-continue-port))
  (assert-raises (peek-char invalid-utf8-no-continue-port))     ; No continuation byte
  (assert-raises (read-char invalid-utf8-no-continue-port))
  (assert-equal #\3 (read-char invalid-utf8-no-continue-port))))  ; Should recover at next character

(define-test "(write-char)" (expect-success
  (define output-string (open-output-string))

  (write-char #\H output-string)
  (write-char #\e output-string)
  (write-char #\l output-string)
  (write-char #\l output-string)

  (parameterize ((current-output-port output-string))
    (write-char #\☃))

  (write-char #\! output-string)
  (assert-equal "Hell☃!" (get-output-string output-string))))

(define-test "(write-char) with invalid character fails" (expect-failure
  (write-char #\x110000 (open-output-string))))

(define-test "(read-line)" (expect-success
  (define test-bytevector (bytevector-append
                            (string->utf8 "ASCII line 1!\n")   ; Pure ASCII
                            (string->utf8 "Unicode line 2‼\n") ; Unicode double exclamation mark
                            (string->utf8 "\n")                ; Empty line
                            #u8(#xe0 #x0a)                     ; Invalid Unicode character
                            (string->utf8 "Final line 3!!!"))) ; No newline character

  (define input-bytevector (open-input-bytevector test-bytevector))

  (assert-equal "ASCII line 1!" (read-line input-bytevector))
  (assert-equal "Unicode line 2‼" (read-line input-bytevector))
  (assert-equal "" (read-line input-bytevector))

  (assert-raises (parameterize ((current-input-port input-bytevector))
                               (read-line)))

  (assert-equal "Final line 3!!!" (read-line input-bytevector))
  (assert-true (eof-object? (read-line input-bytevector)))))

