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

(define-test "(read-u8), (peek-u8), (u8-ready?)" (expect-success
  (define input-string (open-input-string "Hell☃!"))
  (assert-true (u8-ready? input-string))

  (assert-equal #x48 (read-u8 input-string))
  (assert-equal #x65 (read-u8 input-string))
  (assert-true (u8-ready? input-string))

  (assert-equal #x6c (peek-u8 input-string))
  (assert-equal #x6c (peek-u8 input-string))
  (assert-true (u8-ready? input-string))

  (assert-equal #x6c (read-u8 input-string))
  (assert-equal #x6c (read-u8 input-string))
  (assert-true (u8-ready? input-string))

  (parameterize
    ((current-input-port input-string))
    (assert-equal #xe2 (read-u8))
    (assert-equal #x98 (read-u8))
    (assert-equal #x83 (read-u8))
    (assert-true (u8-ready?))

    ; Peeking the last character should still allow it to be read
    (assert-equal #x21 (peek-u8))
    (assert-equal #x21 (read-u8)))

  (assert-true (eof-object? (peek-u8 input-string)))
  (assert-true (eof-object? (read-u8 input-string)))
  (assert-true (u8-ready? input-string))))

(define-test "(u8-ready?) returns false for stdin" (expect-success
  ; There shouldn't be anything to read on stdin
  (assert-false (u8-ready?))))

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

(define-test "(read-char), (peek-char), (char-ready?)" (expect-success
  (import (llambda error))

  (define empty-port (open-input-string ""))
  (assert-true (char-ready? empty-port))
  (assert-true (eof-object? (peek-char empty-port)))
  (assert-true (eof-object? (read-char empty-port)))
  (assert-true (char-ready? empty-port))

  (define ascii-port (open-input-string "Hello!"))
  (parameterize ((current-input-port ascii-port))
    (assert-true (char-ready?))
    (assert-equal #\H (read-char))
    (assert-equal #\e (read-char))
    (assert-equal #\l (read-char))
    (assert-equal #\l (read-char))

    (assert-true (char-ready?))
    (assert-equal #\o (peek-char))
    (assert-equal #\o (peek-char))
    (assert-equal #\o (read-char))

    (assert-true (char-ready?))
    (assert-equal #\! (peek-char))
    (assert-equal #\! (read-char)))

  (assert-true (eof-object? (read-char ascii-port)))

  (define valid-utf8-port (open-input-string "Hell☃!"))
  (assert-true (char-ready? valid-utf8-port))
  (assert-equal #\H (read-char valid-utf8-port))
  (assert-equal #\e (read-char valid-utf8-port))
  (assert-equal #\l (read-char valid-utf8-port))
  (assert-equal #\l (read-char valid-utf8-port))

  (assert-true (char-ready? valid-utf8-port))
  (assert-equal #\☃ (peek-char valid-utf8-port))
  (assert-equal #\☃ (read-char valid-utf8-port))

  (assert-true (char-ready? valid-utf8-port))
  (assert-equal #\! (read-char valid-utf8-port))
  (assert-true (char-ready? valid-utf8-port))

  (define truncated-utf8-port (open-input-bytevector #u8(#x31 #xe2 #x98)))
  (assert-equal #\1 (read-char truncated-utf8-port))
  ; This will EOF due to the port ending mid-byte sequence
  (assert-true (eof-object? (peek-char truncated-utf8-port)))
  ; We should be able to read the remaining bytes using (read-utf8)
  (assert-equal #xe2 (read-u8 truncated-utf8-port))
  (assert-equal #x98 (read-u8 truncated-utf8-port))

  (define invalid-utf8-header-port (open-input-bytevector #u8(#x31 #xff #x33)))
  (assert-equal #\1 (read-char invalid-utf8-header-port))
  (assert-true (char-ready? invalid-utf8-header-port)) ; This should return #t because (read-char) won't block
  (assert-raises utf8-error? (peek-char invalid-utf8-header-port)) ; Invalid header byte
  (assert-raises utf8-error? (read-char invalid-utf8-header-port))
  (assert-equal #\3 (read-char invalid-utf8-header-port)) ; Should recover at next character

  (define invalid-utf8-overlong-port (open-input-bytevector #u8(#x31 #xe0 #x80 #xaf #x33)))
  (assert-equal #\1 (read-char invalid-utf8-overlong-port))
  (assert-true (char-ready? invalid-utf8-overlong-port))
  (assert-raises utf8-error? (peek-char invalid-utf8-overlong-port)) ; Overlong encoding
  (assert-raises utf8-error? (read-char invalid-utf8-overlong-port))
  (assert-equal #\3 (read-char invalid-utf8-overlong-port)) ; Should recover at next character

  (define invalid-utf8-no-continue-port (open-input-bytevector #u8(#x31 #xe2 #x98 #x33)))
  (assert-equal #\1 (read-char invalid-utf8-no-continue-port))
  (assert-true (char-ready? invalid-utf8-no-continue-port))
  (assert-raises utf8-error? (peek-char invalid-utf8-no-continue-port)) ; No continuation byte
  (assert-raises utf8-error? (read-char invalid-utf8-no-continue-port))
  (assert-equal #\3 (read-char invalid-utf8-no-continue-port)))) ; Should recover at next character

(define-test "(char-ready?) returns false for stdin" (expect-success
  ; There shouldn't be anything to read on stdin
  (assert-false (char-ready?))))

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
  (import (llambda error))

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

  (assert-raises utf8-error? (parameterize ((current-input-port input-bytevector))
                                             (read-line)))

  (assert-equal "Final line 3!!!" (read-line input-bytevector))
  (assert-true (eof-object? (read-line input-bytevector)))))

(define-test "(read-bytevector)" (expect-success
  (define test-bytevector #u8(1 2 3 4 5 6 7))
  (define input-bytevector (open-input-bytevector test-bytevector))

  (assert-equal #u8() (read-bytevector 0 input-bytevector))
  (assert-equal #u8(1) (read-bytevector 1 input-bytevector))
  (assert-equal #u8(2 3) (read-bytevector 2 input-bytevector))

  (parameterize ((current-input-port input-bytevector))
    (assert-equal #u8(4 5 6) (read-bytevector 3)))

  (assert-equal #u8(7) (read-bytevector 8 input-bytevector))
  (assert-true (eof-object? (read-bytevector 8 input-bytevector)))
  (assert-true (eof-object? (read-bytevector 0 input-bytevector)))))

(define-test "(read-bytevector) with a negative length fails" (expect-error range-error?
  (define test-bytevector #u8(1 2 3 4 5 6 7))
  (define input-bytevector (open-input-bytevector test-bytevector))

  (read-bytevector -1 input-bytevector)))

(define-test "(read-string)" (expect-success
  (import (llambda error))

  (define valid-utf8-port (open-input-string "Hell☃!"))
  (assert-equal "" (read-string 0 valid-utf8-port))
  (assert-equal "Hell" (read-string 4 valid-utf8-port))

  (parameterize ((current-input-port valid-utf8-port))
    (assert-equal "☃!" (read-string 2)))

  (assert-equal "" (read-string 0 valid-utf8-port))
  (assert-true (eof-object? (read-string 1 valid-utf8-port)))

  (define japanese-port (open-input-string "む姎 媥焯簨盥媯 ビョ禯騪っ鏨 を"))
  (assert-equal "む姎 " (read-string 3 japanese-port))
  (assert-equal "媥焯簨盥媯" (read-string 5 japanese-port))
  (assert-equal " ビョ禯騪っ鏨 " (read-string 8 japanese-port))
  (assert-equal "を" (read-string 1024 japanese-port))
  (assert-true (eof-object? (read-string 1024 japanese-port)))
  (assert-true (eof-object? (read-string 0 japanese-port)))

  (define boundary-condition-port (open-input-bytevector #u8(#x00 #x7f #xc2 #x80 #xdf #xbf #xe0 #xa0 #x80 #xef #xbf #xbf #xf0 #x90 #x80 #x80 #xf4 #x8f #xbf #xbf)))
  (assert-equal "\x00;\x7f;" (read-string 2 boundary-condition-port))
  (assert-equal "\x80;\x7ff;" (read-string 2 boundary-condition-port))
  (assert-equal "\x800;\xffff;" (read-string 2 boundary-condition-port))
  (assert-equal "\x10000;\x10FFFF;" (read-string 2 boundary-condition-port))
  (assert-true (eof-object? (read-string 2 boundary-condition-port)))

  (define invalid-utf8-header-port (open-input-bytevector #u8(#x31 #x32 #xff #x33 #x34 #x35)))
  (assert-raises utf8-error? (read-string 10 invalid-utf8-header-port))
  ; This should continue properly after the error
  (assert-equal "345" (read-string 10 invalid-utf8-header-port))
  (assert-true (eof-object? (read-string 1 invalid-utf8-header-port)))

  (define overlong-encoding-port (open-input-bytevector #u8(#xf0 #x80 #x80 #xaf #xe2 #x98 #x83 #xe0 #x80 #xaf)))
  ; This also mixes (read-char) and (read-string)
  (assert-raises utf8-error? (read-string 10 overlong-encoding-port))
  (assert-equal #\☃ (read-char overlong-encoding-port))
  (assert-raises utf8-error? (read-string 10 overlong-encoding-port))
  (assert-true (eof-object? (read-string 1 overlong-encoding-port)))

  (define truncated-utf8-port (open-input-bytevector #u8(#x31 #x32 #xe2 #x98)))
  (assert-equal "12" (read-string 16 truncated-utf8-port))
  ; For consistency with (read-char) we will discard the incomplete character at the end of the stream
  (assert-true (eof-object? (read-u8 truncated-utf8-port)))

  (define missing-continuation-byte-port (open-input-bytevector #u8(#x31 #x32 #xe2 #x98 #x33 #x34)))
  (assert-raises utf8-error? (read-string 10 missing-continuation-byte-port))
  (assert-equal "34" (read-string 10 missing-continuation-byte-port))
  (assert-true (eof-object? (read-string 0 missing-continuation-byte-port)))))

(define-test "(read-string) with a negative length fails" (expect-error range-error?
  (define test-bytevector #u8(1 2 3 4 5 6 7))
  (define input-bytevector (open-input-bytevector test-bytevector))

  (read-string -1 input-bytevector)))

(define-test "(write-string)" (expect-success
  (define test-string "む姎 媥焯簨盥媯 ビョ禯騪っ鏨 を")

  (define entire-output-port (open-output-string))
  (parameterize ((current-output-port entire-output-port))
    (write-string test-string))
  (assert-equal test-string (get-output-string entire-output-port))

  (define another-entire-output-port (open-output-string))
  (write-string test-string another-entire-output-port)
  (assert-equal test-string (get-output-string another-entire-output-port))

  (define start-only-output-port (open-output-string))
  (write-string test-string start-only-output-port 5)
  (assert-equal "簨盥媯 ビョ禯騪っ鏨 を" (get-output-string start-only-output-port))

  (define start-end-output-port (open-output-string))
  (write-string test-string start-end-output-port 6 12)
  (assert-equal "盥媯 ビョ禯" (get-output-string start-end-output-port))))

(define-test "(write-string) with backwards slice fails" (expect-error range-error?
  (define output-port (open-output-string))
  (write-string "1☃3" output-port 2 1)))

(define-test "(write-string) past end of string fails" (expect-error range-error?
  (define output-port (open-output-string))
  (write-string "1☃3" output-port 0 4)))

(define-test "(write-string) with negative start index fails" (expect-error range-error?
  (define output-port (open-output-string))
  (write-string "1☃3" output-port -1)))

(define-test "(write-bytevector)" (expect-success
  (define test-bytevector #u8(0 1 2 3 4 5 6 7 8 9))

  (define entire-output-port (open-output-bytevector))
  (parameterize ((current-output-port entire-output-port))
    (write-bytevector test-bytevector))
  (assert-equal test-bytevector (get-output-bytevector entire-output-port))

  (define start-only-output-port (open-output-bytevector))
  (write-bytevector test-bytevector start-only-output-port 5)
  (assert-equal #u8(5 6 7 8 9) (get-output-bytevector start-only-output-port))

  (define start-end-output-port (open-output-bytevector))
  (write-bytevector test-bytevector start-end-output-port 6 8)
  (assert-equal #u8(6 7) (get-output-bytevector start-end-output-port))))

(define-test "(write-bytevector) with backwards slice fails" (expect-error range-error?
  (define output-port (open-output-string))
  (write-bytevector #u8(1 2 3) output-port 2 1)))

(define-test "(write-bytevector) past end of string fails" (expect-error range-error?
  (define output-port (open-output-string))
  (write-bytevector #u8(1 2 3) output-port 0 4)))

(define-test "(write-bytevector) with negative start index fails" (expect-error range-error?
  (define output-port (open-output-string))
  (write-bytevector #u8(1 2 3) output-port -1)))

(define-test "(flush-output-port)" (expect-output (ABC)
  (import (scheme process-context))

  ; This isn't guaranteed to fail if (flush-output-port) is a no-op. However, it's likely to on most buffered
  ; implementations
  (write-string "ABC")
  (flush-output-port)
  (emergency-exit #t)))

(define-test "(read-bytevector!)" (expect-success
  (define test-bytevector (make-bytevector 4))

  (define input-port (open-input-bytevector #u8(1 2 3 4 5 6 7 8 9 10 11 12)))

  (parameterize ((current-input-port input-port))
    (assert-equal 4 (read-bytevector! test-bytevector)))

  (assert-equal #u8(1 2 3 4) test-bytevector)

  (assert-equal 4 (read-bytevector! test-bytevector input-port))
  (assert-equal #u8(5 6 7 8) test-bytevector)

  (assert-equal 2 (read-bytevector! test-bytevector input-port 2))
  (assert-equal #u8(5 6 9 10) test-bytevector)

  (assert-equal 2 (read-bytevector! test-bytevector input-port 0 2))
  (assert-equal #u8(11 12 9 10) test-bytevector)

  (assert-equal 0 (read-bytevector! test-bytevector input-port 0 0))
  (assert-equal 0 (read-bytevector! test-bytevector input-port 4 4))

  (assert-true (eof-object? (read-bytevector! test-bytevector input-port)))))

(define-test "(read-bytevector!) on bytevector literal fails" (expect-error mutate-literal-error?
  (define test-bytevector #u8(0 0 0 0))

  (define input-port (open-input-bytevector #u8(1 2 3 4 5 6 7 8 9 10 11 12)))
  (read-bytevector! test-bytevector input-port)))

(define-test "(read-bytevector!) with backwards slice fails" (expect-error range-error?
  (define test-bytevector (make-bytevector 4))

  (define input-port (open-input-bytevector #u8(1 2 3 4 5 6 7 8 9 10 11 12)))
  (read-bytevector! test-bytevector input-port 3 2)))

(define-test "(read-bytevector!) past end of bytevector fails" (expect-error range-error?
  (define test-bytevector (make-bytevector 4))

  (define input-port (open-input-bytevector #u8(1 2 3 4 5 6 7 8 9 10 11 12)))
  (read-bytevector! test-bytevector input-port 2 5)))

(define-test "(read-bytevector!) with negative start index fails" (expect-error range-error?
  (define test-bytevector (make-bytevector 4))

  (define input-port (open-input-bytevector #u8(1 2 3 4 5 6 7 8 9 10 11 12)))
  (read-bytevector! test-bytevector input-port -2)))
