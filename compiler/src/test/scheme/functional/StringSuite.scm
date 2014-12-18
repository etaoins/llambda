; This test assumes the inline -> heap transition happens after 12 bytes
(define-test "string constant is string" (expect #t
	(string? "Hello, world!")))

(define-test "string constant of maximum inline size" (expect "crash-length"
	"crash-length"))

(define-test "empty list is not string" (expect #f
	(string? '())))

(define-test "make empty string" (expect ""
	(make-string 0 #\null)))

(define-test "make non-empty string" (expect "aaaaa"
	(make-string 5 #\a)))

(define-test "make string with invalid fill character fails" (expect-failure
	(make-string 5 #\x110000)))

(define-test "(string) with no arguments" (expect ""
	(string)))

(define-test "(string) with Unicode arguments" (expect "Hell☃!"
	(string #\H #\e #\l #\l #\x2603 #\!)))

(define-test "(string) with invalid character fails" (expect-failure
	(string #\H #\e #\l #\x110000 #\x2603 #\!)))

(define-test "list->string with empty list" (expect ""
	(list->string '())))

(define-test "list->string with Unicode chars" (expect "Hell☃!"
	(list->string '(#\H #\e #\l #\l #\x2603 #\!))))

(define-test "list->string with invalid character fails" (expect-failure
	(list->string '(#\H #\e #\l #\l #\x110000 #\!))))

(define-test "length of empty string" (expect 0
	(string-length "")))

(define-test "length of ASCII string" (expect 5
	(string-length "Hello")))
 
(define-test "length of BMP Unicode string" (expect 6
	(string-length "Hell☃!")))

(define-test "length of non-BMP Unicode string" (expect 6
	(string-length "Hell🏂!")))

(define-test "string-ref on ASCII character" (expect #\e
	(string-ref "Hell☃!" 1)))

(define-test "string-ref on BMP Unicode character" (expect #\x2603
	(string-ref "Hell☃!" 4)))

(define-test "string-ref on non-BMP Unicode literal character" (expect #\x1f3c2
	(string-ref "Hell🏂!" 4)))

(define-test "string-ref on non-BMP Unicode escaped character" (expect #\x1f3c2
	(string-ref "Hell\x1f3c2;" 4)))

(define-test "string-ref past end of string fails" (expect-error range-error?
	(string-ref "Hell☃!" 10)))

(define-test "string-ref with negative index fails" (expect-error range-error?
	(string-ref "Hell☃!" -1)))

(define-test "string-set! of ASCII character" (expect "*!*"
	(define test-string (make-string 3 #\*))
	(string-set! test-string 1 #\!)
	test-string))

(define-test "string-set! on string literal fails" (expect-error mutate-literal-error?
	(string-set! "I'm constant" 1 #\!)))

(define-test "string-set! of Unicode character" (expect "**☃"
	(define test-string (make-string 3 #\*))
	(string-set! test-string 2 #\x2603)
	test-string))

(define-test "string-set! on an inline string creating a heap string" (expect "☃***********"
	(define test-string (make-string 12 #\*))
	(string-set! test-string 0 #\x2603)
	test-string))

(define-test "string-set! past end of string fails" (expect-error range-error?
	(define test-string (make-string 3 #\*))
	(string-set! test-string 4 #\x2603)
	test-string))

(define-test "string-set! with negative index fails" (expect-error range-error?
	(define test-string (make-string 3 #\*))
	(string-set! test-string -1 #\x2603)
	test-string))

(define-test "string-set! with invalid character fails" (expect-failure
	(define test-string (make-string 3 #\*))
	(string-set! test-string 0 #\x110000)
	test-string))

(define-test "string-append of no strings" (expect ""
	(string-append)))

(define-test "string-append of one string" (expect "Hello"
	(string-append "Hello")))

(define-test "string-append of three strings" (expect-success
  (define new-string (string-append "Hell" "☃" "!"))

  (assert-equal "Hell☃!" new-string)
  (assert-equal 6 (string-length new-string))))

(define-test "string-append of boolean fails" (expect-error type-error?
	(string-append "Hell" "☃" "!" #f)))

(define-test "(string->list)" (expect-success
  (assert-equal '(#\H #\e #\l #\l #\x2603 #\!) (string->list "Hell☃!"))
  (assert-equal '(#\l #\l #\x2603 #\!) (string->list "Hell☃!" 2))
  (assert-equal '(#\l #\l) (string->list "Hell☃!" 2 4))
  (assert-equal '() (string->list "Hell☃!" 0 0))
  (assert-equal '() (string->list "Hell☃!" 6 6))))

(define-test "(string->list) with backwards slice fails" (expect-error range-error?
  (string->list "Hell☃!" 2 1)))

(define-test "(string->list) past end of string fails" (expect-error range-error?
  (string->list "Hell☃!" 0 8)))

(define-test "(string->list) with negative start index fails" (expect-error range-error?
  (string->list "Hell☃!" -1)))

(define-test "(string-copy)" (expect-success
  (assert-equal "" (string-copy ""))
  (assert-equal "1☃3" (string-copy "1☃3"))
  (assert-equal "☃3" (string-copy "1☃3" 1))
  (assert-equal "☃" (string-copy "1☃3" 1 2))
  (assert-equal "" (string-copy "1☃3" 0 0))
  (assert-equal "" (string-copy "1☃3" 3 3))

  (define a "18☃8") ; a may be immutable
  (define b (string-copy a))
  (string-set! b 0 #\Я) ; b is mutable

  ; Make sure a was preserved
  (assert-equal "18☃8" a)

  (assert-equal "Я8☃8" b)
  (define c (string-copy b 1 3))
  (assert-equal "8☃" c)))

(define-test "(string-copy) with backwards slice fails" (expect-error range-error?
  (string-copy "1☃3" 2 1)))

(define-test "(string-copy) past end of string fails" (expect-error range-error?
  (string-copy "1☃3" 0 4)))

(define-test "(string-copy) with negative start index fails" (expect-error range-error?
  (string-copy "1☃3" -1)))

(define-test "(substring)" (expect-success
  (assert-equal "日本国" (substring "日本国" 0 3))
  (assert-equal "本" (substring "日本国" 1 2))
  (assert-equal "" (substring "日本国" 0 0))
  (assert-equal "" (substring "日本国" 3 3))))

(define-test "(string-upcase)" (expect-success
  (import (scheme char))

  (assert-equal "" (string-upcase ""))
  (assert-equal "HELL☃ WORLDS" (string-upcase "hell☃ worldſ"))
  (assert-equal "HELLO W☃RLDS" (string-upcase "HELLO W☃RLDſ"))
  (assert-equal "日本国" (string-upcase "日本国"))))

(define-test "(string-downcase)" (expect-success
  (import (scheme char))

  (assert-equal "" (string-downcase ""))
  (assert-equal "hell☃ worldſ" (string-downcase "hell☃ worldſ"))
  (assert-equal "hello w☃rldſ" (string-downcase "HELLO W☃RLDſ"))
  (assert-equal "日本国" (string-downcase "日本国"))))

(define-test "(string-foldcase)" (expect-success
  (import (scheme char))

  (assert-equal "" (string-foldcase ""))
  (assert-equal "hell☃ worlds" (string-foldcase "hell☃ worldſ"))
  (assert-equal "hello w☃rlds" (string-foldcase "HELLO W☃RLDſ"))
  (assert-equal "日本国" (string-foldcase "日本国"))))

(define-test "(string=?)" (expect-success
  (assert-true  (string=? "hello" "hello"))
  (assert-false (string=? "hello" "HELLO"))
  (assert-false (string=? "HELLO" "hello"))
  (assert-false (string=? "hello" "hello!"))
  (assert-false (string=? "hello!" "hello"))
  (assert-true  (string=? "日本国" "日本国"))))

(define-test "(string<?)" (expect-success
  (assert-false (string<? "hello" "hello"))
  (assert-false (string<? "hello" "HELLO"))
  (assert-true  (string<? "HELLO" "hello"))
  (assert-true  (string<? "hello" "hello!"))
  (assert-false (string<? "hello!" "hello"))
  (assert-false (string<? "日本国" "日本国"))))

(define-test "(string>?)" (expect-success
  (assert-false (string>? "hello" "hello"))
  (assert-true  (string>? "hello" "HELLO"))
  (assert-false (string>? "HELLO" "hello"))
  (assert-false (string>? "hello" "hello!"))
  (assert-true  (string>? "hello!" "hello"))
  (assert-false (string>? "日本国" "日本国"))))

(define-test "(string<=?)" (expect-success
  (assert-true  (string<=? "hello" "hello"))
  (assert-false (string<=? "hello" "HELLO"))
  (assert-true  (string<=? "HELLO" "hello"))
  (assert-true  (string<=? "hello" "hello!"))
  (assert-false (string<=? "hello!" "hello"))
  (assert-true  (string<=? "日本国" "日本国"))))

(define-test "(string>=?)" (expect-success
  (assert-true  (string>=? "hello" "hello"))
  (assert-true  (string>=? "hello" "HELLO"))
  (assert-false (string>=? "HELLO" "hello"))
  (assert-false (string>=? "hello" "hello!"))
  (assert-true  (string>=? "hello!" "hello"))
  (assert-true  (string>=? "日本国" "日本国"))))

(define-test "(string-ci=?)" (expect-success
  (import (scheme char))

  (assert-true  (string-ci=? "hello" "hello"))
  (assert-true  (string-ci=? "hello" "HELLO"))
  (assert-true  (string-ci=? "HELLO" "hello"))
  (assert-false (string-ci=? "hello" "hello!"))
  (assert-false (string-ci=? "hello!" "hello"))
  (assert-true  (string-ci=? "日本国" "日本国"))))

(define-test "(string-ci<?)" (expect-success
  (import (scheme char))

  (assert-false (string-ci<? "hello" "hello"))
  (assert-false (string-ci<? "hello" "HELLO"))
  (assert-false (string-ci<? "HELLO" "hello"))
  (assert-true  (string-ci<? "hello" "hello!"))
  (assert-false (string-ci<? "hello!" "hello"))
  (assert-false (string-ci<? "日本国" "日本国"))))

(define-test "(string-ci>?)" (expect-success
  (import (scheme char))

  (assert-false (string-ci>? "hello" "hello"))
  (assert-false (string-ci>? "hello" "HELLO"))
  (assert-false (string-ci>? "HELLO" "hello"))
  (assert-false (string-ci>? "hello" "hello!"))
  (assert-true  (string-ci>? "hello!" "hello"))
  (assert-false (string-ci>? "日本国" "日本国"))))

(define-test "(string-ci<=?)" (expect-success
  (import (scheme char))

  (assert-true  (string-ci<=? "hello" "hello"))
  (assert-true  (string-ci<=? "hello" "HELLO"))
  (assert-true  (string-ci<=? "HELLO" "hello"))
  (assert-true  (string-ci<=? "hello" "hello!"))
  (assert-false (string-ci<=? "hello!" "hello"))
  (assert-true  (string-ci<=? "日本国" "日本国"))))

(define-test "(string-ci>=?)" (expect-success
  (import (scheme char))

  (assert-true  (string-ci>=? "hello" "hello"))
  (assert-true  (string-ci>=? "hello" "HELLO"))
  (assert-true  (string-ci>=? "HELLO" "hello"))
  (assert-false (string-ci>=? "hello" "hello!"))
  (assert-true  (string-ci>=? "hello!" "hello"))
  (assert-true  (string-ci>=? "日本国" "日本国"))))

(define-test "(string-copy!)" (expect-success
  (define a "12345")
  (define b (string-copy "abcde"))
  (string-copy! b 1 a 0 2)

  (assert-equal "a12de" b)

  (string-copy! b 1 a 0 0)
  (string-copy! b 1 a 5)
  (assert-equal "a12de" b)

  (string-copy! b 0 a)
  (assert-equal "12345" b)))

(define-test "(string-copy!) on string literal fails" (expect-error mutate-literal-error?
  (define a "12345")
  (define b "abcde")
  (string-copy! b 1 a 0 2)

  (assert-equal "a12de" b)))

(define-test "(string-copy!) with negative start index fails" (expect-error range-error?
  (define a "12345")
  (define b (string-copy "abcde"))

  (string-copy! b 1 a -1 2)))

(define-test "(string-copy!) with backwards slice fails" (expect-error range-error?
  (define a "12345")
  (define b (string-copy "abcde"))
  (string-copy! b 1 a 2 0)))

(define-test "(string-copy!) past end of from fails" (expect-error range-error?
  (define a "12345")
  (define b (string-copy "abcde"))
  (string-copy! b 2 a 4 6)))

(define-test "(string-copy!) past end of to fails" (expect-error range-error?
  (define a "12345")
  (define b (string-copy "abcde"))
  (string-copy! b 2 a 1)))

(define-test "(string-fill!)" (expect-success
  (define test-string (string-copy "01234567"))

  (string-fill! test-string #\☃ 0 0)
  (string-fill! test-string #\☃ 4 6)
  (assert-equal "0123☃☃67" test-string)

  (string-fill! test-string #\λ 8 8)
  (string-fill! test-string #\λ 6)
  (assert-equal "0123☃☃λλ" test-string)

  (string-fill! test-string #\ℵ)
  (assert-equal "ℵℵℵℵℵℵℵℵ" test-string)

  (string-fill! test-string #\0 2 5)
  (assert-equal "ℵℵ000ℵℵℵ" test-string)))

(define-test "(string-fill!) with backward slice fails" (expect-error range-error?
  (define test-string (string-copy "01234567"))
  (string-fill! test-string #\☃ 6 4)))

(define-test "(string-fill!) with negative start index fails" (expect-error range-error?
  (define test-string (string-copy "01234567"))
  (string-fill! test-string #\☃ -1 1)))

(define-test "(string-fill!) past end of string fails" (expect-error range-error?
  (define test-string (string-copy "01234567"))
  (string-fill! test-string #\☃ 9)))

(define-test "(string-fill!) with invalid fill character fails" (expect-failure
  (define test-string (string-copy "01234567"))
  (string-fill! test-string #\x110000)))
