(define-test "string constant is string" (expect #t
	(import (scheme core))
	(string? "Hello, world!")))

(define-test "empty list is not string" (expect #f
	(import (scheme core))
	(string? '())))

(define-test "make empty string" (expect ""
	(import (scheme core))
	(make-string 0 #\null)))

(define-test "make non-empty string" (expect "aaaaa"
	(import (scheme core))
	(make-string 5 #\a)))

(define-test "(string) with no arguments" (expect ""
	(import (scheme core))
	(string)))

(define-test "(string) with Unicode arguments" (expect "Hell☃!"
	(import (scheme core))
	(string #\H #\e #\l #\l #\x2603 #\!)))

(define-test "list->string with empty list" (expect ""
	(import (scheme core))
	(list->string '())))

(define-test "list->string with Unicode chars" (expect "Hell☃!"
	(import (scheme core))
	(list->string '(#\H #\e #\l #\l #\x2603 #\!))))

(define-test "length of empty string" (expect 0
	(import (scheme core))
	(string-length "")))

(define-test "length of ASCII string" (expect 5
	(import (scheme core))
	(string-length "Hello")))
 
(define-test "length of Unicode string" (expect 6
	(import (scheme core))
	(string-length "Hell☃!")))

(define-test "string-ref on ASCII character" (expect #\e
	(import (scheme core))
	(string-ref "Hell☃!" 1)))

(define-test "string-ref on Unicode character" (expect #\x2603
	(import (scheme core))
	(string-ref "Hell☃!" 4)))

(define-test "string-ref past end of string" (expect-failure
	(import (scheme core))
	(string-ref "Hell☃!" 10)))

(define-test "string-set! of ASCII character" (expect "*!*"
	(import (scheme core))
	(define test-string (make-string 3 #\*))
	(string-set! test-string 1 #\!)
	test-string))

(define-test "string-set! of Unicode character" (expect "**☃"
	(import (scheme core))
	(define test-string (make-string 3 #\*))
	(string-set! test-string 2 #\x2603)
	test-string))

(define-test "string-set! past end of string" (expect-failure
	(import (scheme core))
	(define test-string (make-string 3 #\*))
	(string-set! test-string 4 #\x2603)
	test-string))

(define-test "string-append of no strings" (expect ""
	(import (scheme core))
	(string-append)))

(define-test "string-append of one string" (expect "Hello"
	(import (scheme core))
	(string-append "Hello")))

(define-test "string-append of three strings" (expect "Hell☃!"
	(import (scheme core))
	(string-append "Hell" "☃" "!")))

(define-test "string-append of boolean fails" (expect-failure
	(import (scheme core))
	(string-append "Hell" "☃" "!" #f)))
