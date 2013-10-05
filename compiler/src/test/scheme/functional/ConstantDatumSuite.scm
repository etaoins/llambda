(define-test "string" (expect "Hello, world!"
	"Hello, world!"))

(define-test "exact integer" (expect 31337
	31337))

(define-test "inexact rational" (expect -12.0
	-12.0))

(define-test "positive infinity" (expect +inf.0
	+inf.0))

(define-test "negative infinity" (expect +inf.0
	+inf.0))

(define-test "boolean true" (expect #t
	#t))

(define-test "boolean false" (expect #f
	#f))

(define-test "symbol" (expect symbol
	; Need (scheme core) for quote
	(import (scheme core))
	'symbol))

(define-test "unspecific" (expect #!unspecific
	(import (scheme core))
	'#!unspecific))

(define-test "empty list" (expect ()
	(import (scheme core))
	'()))

(define-test "character" (expect #\a
	#\a))

(define-test "proper list" (expect (1 2 3 4)
	(import (scheme core))
	'(1 2 3 4)))

(define-test "improper list" (expect (1 2 3 . 4)
	(import (scheme core))
	'(1 2 3 . 4)))

(define-test "vector" (expect #(a b c) 
	#(a b c)))

(define-test "bytevector" (expect #u8(0 1 127 128 255) 
	#u8(0 1 127 128 255)))

(define-test "complex structure" (expect (0 . (#t #(a #\b #u8(3)) 4 #("five" "six") 7.0 #t))
	(import (scheme core))
	'(0 . (#t #(a #\b #u8(3)) 4 #("five" "six") 7.0 #t)) ))
