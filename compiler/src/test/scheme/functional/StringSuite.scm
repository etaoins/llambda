(define-test "(string?)" (expect-static-success
  (assert-true  (string? "Hello, world!"))
  (assert-false (string? '()))))

; This test assumes the inline -> heap transition happens after 28 bytes
(define-test "string constant of maximum inline size" (expect "this-string-is-28-bytes-long"
  "this-string-is-28-bytes-long"))

(define-test "(make-string)" (expect-success
  (assert-equal "" (make-string 0 #\null))
  (assert-equal "aaaaa" (make-string 5 #\a))))

(define-test "(make-string) with negative length fails" (expect-error range-error?
  (make-string -5 #\a)))

(define-test "(string)" (expect-success
  (assert-equal "" (string))
  (assert-equal "Hell☃!" (string #\H #\e #\l #\l #\x2603 #\!))))

(define-test "(list->string)" (expect-success
  (assert-equal "" (list->string '()))
  (assert-equal "Hell☃!"  (list->string '(#\H #\e #\l #\l #\x2603 #\!)))))

(define-test "(string-length)" (expect-static-success
  (assert-equal 0 (string-length ""))
  (assert-equal 5 (string-length "Hello"))
  (assert-equal 6 (string-length "Hell☃!"))
  (assert-equal 6 (string-length "Hell🏂!"))))

(define-test "(string-ref)" (expect-success
  (assert-equal #\e (string-ref "Hell☃!" 1))
  (assert-equal #\x2603 (string-ref "Hell☃!" 4))
  (assert-equal #\x1f3c2 (string-ref "Hell🏂 !" 4))
  (assert-equal #\x1f3c2 (string-ref "Hell\x1f3c2;" 4))))

(define-test "(string-ref) past end of string fails" (expect-error range-error?
  (string-ref "Hell☃!" 10)))

(define-test "(string-ref) with negative index fails" (expect-error range-error?
  (string-ref "Hell☃!" -1)))

(define-test "(string-set!) of ASCII character" (expect "*!*"
  (define test-string (make-string 3 #\*))
  (string-set! test-string 1 #\!)
  test-string))

(define-test "(string-set!) on string literal fails" (expect-error mutate-literal-error?
  (string-set! "I'm constant" 1 #\!)))

(define-test "(string-set!) of Unicode character" (expect "**☃"
  (define test-string (make-string 3 #\*))
  (string-set! test-string 2 #\x2603)
  test-string))

(define-test "(string-set!) on an inline string creating a heap string" (expect "☃his-string-is-28-bytes-long"
  (define test-string (string-copy "this-string-is-28-bytes-long"))
  (string-set! test-string 0 #\x2603)
  test-string))

(define-test "(string-set!) past end of string fails" (expect-error range-error?
  (define test-string (make-string 3 #\*))
  (string-set! test-string 4 #\x2603)
  test-string))

(define-test "(string-set!) with negative index fails" (expect-error range-error?
  (define test-string (make-string 3 #\*))
  (string-set! test-string -1 #\x2603)
  test-string))

(define-test "(string-append)" (expect-success
  (assert-equal "" (string-append))
  (assert-equal "Hello" (string-append "Hello"))

  (define new-string (string-append "Hell" "☃" "!"))
  (assert-equal "Hell☃!" new-string)
  (assert-equal 6 (string-length new-string))))

(define-test "(string-append) of boolean fails" (expect-error type-error?
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

(define-test "string (hash)" (expect-success
  (import (llambda hash-map))

  (define test-string (make-string 20 #\a))

  ; Calculate a hash value and make sure its stable
  (define first-hash-value (hash test-string))
  (assert-equal first-hash-value (hash test-string))

  (string-set! test-string 0 #\b)
  (assert-false (equal? first-hash-value (hash test-string)))))
