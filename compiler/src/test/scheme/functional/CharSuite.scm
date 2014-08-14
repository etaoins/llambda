(define-test "'3' is a character" (expect #t
	(char? #\3)))

(define-test "number 3 is not a character" (expect #f
	(char? 3)))

(define-test "digit value of '3' is 3" (expect 3
	(digit-value #\3)))

(define-test "digit value of x0664 is 4" (expect 4
	(digit-value #\x0664)))

(define-test "digit value of x0AE6 is 4" (expect 0
	(digit-value #\x0AE6)))

(define-test "digit value of x0EA6 is 4" (expect #f
	(digit-value #\x0EA6)))

(define-test "digit value of x0EA6 is #f" (expect #f
	(digit-value #\x0EA6)))

(define-test "integer value of of x41" (expect #x41
	(char->integer #\x41)))

(define-test "integer value of of x4141" (expect #x4141
	(char->integer #\x4141)))

(define-test "character value of of x41" (expect #\x41
	(integer->char #x41)))

(define-test "character value of of x4141" (expect #\x4141
	(integer->char #x4141)))
