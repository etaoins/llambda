(define-test "'3' is a character" (expect #t
	(import (scheme core))
	(char? #\3)))

(define-test "number 3 is not a character" (expect #f
	(import (scheme core))
	(char? 3)))

(define-test "digit value of '3' is 3" (expect 3
	(import (scheme core))
	(digit-value #\3)))

(define-test "digit value of x0664 is 4" (expect 4
	(import (scheme core))
	(digit-value #\x0664)))

(define-test "digit value of x0AE6 is 4" (expect 0
	(import (scheme core))
	(digit-value #\x0AE6)))

(define-test "digit value of x0EA6 is 4" (expect #f
	(import (scheme core))
	(digit-value #\x0EA6)))

(define-test "digit value of x0EA6 is #f" (expect #f
	(import (scheme core))
	(digit-value #\x0EA6)))

(define-test "integer value of of x41" (expect #x41
	(import (scheme core))
	(char->integer #\x41)))

(define-test "integer value of of x4141" (expect #x4141
	(import (scheme core))
	(char->integer #\x4141)))

(define-test "character value of of x41" (expect #\x41
	(import (scheme core))
	(integer->char #x41)))

(define-test "character value of of x4141" (expect #\x4141
	(import (scheme core))
	(integer->char #x4141)))
