(define-test "(read)" (expect-success
  (import (scheme read))
  (import (llambda error))

  (define (open-input-port source)
    (if (string? source)
      (open-input-string source)
      (open-input-bytevector source)))

  (define-syntax assert-parses
    (syntax-rules ()
                  ((assert-parses expected source)
                   (call-with-port (open-input-port source)
                                   (lambda (string-port)
                                     (assert-equal expected (read string-port)))))))

  (define-syntax assert-parse-raises
    (syntax-rules ()
                  ((assert-invalid-parse pred? source)
                   (call-with-port (open-input-port source)
                                   (lambda (string-port)
                                     (assert-raises pred? (read string-port)))))))

  ; This has a more extensive unit test in C++ in test-datumreader.cpp
  (assert-parses 256 "256")
  (assert-parses 1/4096 "1/4096")
  (assert-parses 256.5 "256.5")

  (assert-parses -7898 "-7898")
  (assert-parses -7898.5 "-7898.5")
  (assert-parses -16/32 "-16/32")

  (assert-parses +inf.0 "+inf.0")
  (assert-parses -inf.0 "-inf.0")
  (assert-parses +nan.0 "+nan.0")

  (assert-parses '|Hello, world!| "|Hello, world!|")
  (assert-parses '|Hello(world!| "|Hello(world!|")
  (assert-parses '|Hello)world!| "|Hello)world!|")
  (assert-parses '|Hello'world!| "|Hello'world!|")
  (assert-parses '|Hello`world!| "|Hello`world!|")
  (assert-parses '|Hello,world!| "|Hello,world!|")

  (assert-parses "Hello\nworld!" "\"Hello\\nworld!\"")
  (assert-parses "Hello☃world!" "\"Hello\x2603;world!\"")
  (assert-parses "Hello☃world!" "\"Hello☃world!\"")

  ; Invalid UTF-8
  (assert-parse-raises utf8-error? #u8(#x22 #xfe #x22))

  (assert-parses '(foo bar baz) "(foo bar baz)")
  (assert-parses '(foo [foobar foobaz 12]) "(foo [foobar foobaz 12])")

  (assert-parses '(quote a) "'a")
  (assert-parses '(quasiquote a) "`a")
  (assert-parses '(unquote a) ",a")
  (assert-parses '(unquote-splicing a) ",@a")

  (assert-parse-raises read-error? "#")

  (assert-parses 5 "#b101")
  (assert-parses -7 "#b-111")

  (assert-parses 31 "#o37")
  (assert-parses -1997 "#o-3715")

  (assert-parses 3735928559 "#xDEADBEEF")
  (assert-parses -609530670837 "#x-8DEADBEEF5")

  (assert-parses #(one two 3) "#(one two 3)")

  (assert-parses #u8(0 127 255) "#u8(0 #x7f 255)")

  (assert-parses #\newline "#\\newline")
  (assert-parses #\☃ "#\\☃")
  (assert-parses #\☃ "#\\x2603")
  (assert-parse-raises read-error? "#\\SPACE")

  ; Invalid UTF-8
  (assert-parse-raises utf8-error? #u8(#x23 #x5c #xfe))

  (assert-parses #!unit "#!unit")
  (assert-parse-raises read-error? "#!notathing")

  (assert-parses #f "#f")
  (assert-parses #f "#false")
  (assert-parses #t "#t")
  (assert-parses #t "#true")))
