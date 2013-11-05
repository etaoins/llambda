(define-test "string constant is string" (expect #t
	(string? "Hello, world!")))

(define-test "empty list is not string" (expect #f
	(string? '())))

(define-test "make empty string" (expect ""
	(make-string 0 #\null)))

(define-test "make non-empty string" (expect "aaaaa"
	(make-string 5 #\a)))

(define-test "(string) with no arguments" (expect ""
	(string)))

(define-test "(string) with Unicode arguments" (expect "Hell☃!"
	(string #\H #\e #\l #\l #\x2603 #\!)))

(define-test "list->string with empty list" (expect ""
	(list->string '())))

(define-test "list->string with Unicode chars" (expect "Hell☃!"
	(list->string '(#\H #\e #\l #\l #\x2603 #\!))))

(define-test "length of empty string" (expect 0
	(string-length "")))

(define-test "length of ASCII string" (expect 5
	(string-length "Hello")))
 
(define-test "length of Unicode string" (expect 6
	(string-length "Hell☃!")))

(define-test "string-ref on ASCII character" (expect #\e
	(string-ref "Hell☃!" 1)))

(define-test "string-ref on Unicode character" (expect #\x2603
	(string-ref "Hell☃!" 4)))

(define-test "string-ref past end of string" (expect-failure
	(string-ref "Hell☃!" 10)))

(define-test "string-set! of ASCII character" (expect "*!*"
	(define test-string (make-string 3 #\*))
	(string-set! test-string 1 #\!)
	test-string))

(define-test "string-set! of Unicode character" (expect "**☃"
	(define test-string (make-string 3 #\*))
	(string-set! test-string 2 #\x2603)
	test-string))

(define-test "string-set! past end of string" (expect-failure
	(define test-string (make-string 3 #\*))
	(string-set! test-string 4 #\x2603)
	test-string))

(define-test "string-append of no strings" (expect ""
	(string-append)))

(define-test "string-append of one string" (expect "Hello"
	(string-append "Hello")))

(define-test "string-append of three strings" (expect "Hell☃!"
	(string-append "Hell" "☃" "!")))

(define-test "string-append of boolean fails" (expect-failure
	(string-append "Hell" "☃" "!" #f)))
