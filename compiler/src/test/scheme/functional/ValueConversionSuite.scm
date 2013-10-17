(define-test "datum can be cast to pair" (expect 1
	(import (scheme core))
	; This assumes (vector-ref) takes a boxed pair
	(vector-ref (car '(#(1) . #f)) 0)))

(define-test "#false can be unboxed as boolean" (expect #t
	(import (scheme core))
	; This assumes (not) takes an unboxed boolean
	(not (car '(#f . #f)))))

(define-test "#true can be unboxed as boolean" (expect #f
	(import (scheme core))
	; This assumes (not) takes an unboxed boolean
	(not (car '(#t . #f)))))

(define-test "empty list can be unboxed as boolean" (expect #f
	(import (scheme core))
	; This assumes (not) takes an unboxed boolean
	(not (car '('() . #f)))))

(define-test "exact int can be unboxed as integer" (expect #(#t #t #t)
	(import (scheme core))
	; This assumes (make-vector) takes an unboxed exact integer
	(make-vector (car '(3 . #f)) #t)))

(define-test "inexact rational cannot be unboxed as integer" (expect-failure
	(import (scheme core))
	; This assumes (make-vector) takes an unboxed exact integer
	(make-vector (car '(3.0 . #f)) #t)))

(define-test "inexact rational can be unboxed as double" (expect 1.0
	(import (scheme core))
	; This assumes (cos) takes an unboxed double
	(cos (car '(0.0 . #f)))))

(define-test "exact integer can be unboxed as double" (expect 1.0
	(import (scheme core))
	; This assumes (cos) takes an unboxed double
	(cos (car '(0 . #f)))))

(define-test "'3' can be unboxed as a character" (expect 3
	(import (scheme core))
	; This assumes (digit-value) takes an unboxed Unicode character
	(digit-value (car '(#\3 . #f)))))

(define-test "string can be unboxed as a UTF-8 C string" (expect 6
	(import (scheme core))
	(import (llambda nfi))
	; Nothing in our stdlib takes UTF-8 C strings because they're binary unsafe
	; and require O(n) importing to determine their length/non-ASCII content.
	; Use strlen from the C standard library for this test
	(define strlen (native-function "strlen" (utf8-cstring) int64))
	(strlen (car '("Hello!" . #f)))))

(define-test "unboxed int 0 converts to unboxed boolean true" (expect #f
	(import (scheme core))
	; This assumes (exact) returns an unboxed integer and (not) takes an unboxed boolean
	(not (exact 0))))

; This seems stupid but it was actually broken at one point
(define-test "unboxed boolean false can be passed to a procedure" (expect #t
	(import (scheme core))
	; This assumes (not) takes and returns an unboxed boolean
	(not (not #t))))

; Make sure if we use type analysis to short circuit bool evaluation do it right
; This was also broken at one point
(define-test "types that cannot be boolean evaluate as true" (expect #f
	(import (scheme core))
	; This assumes (not) takes an unboxed boolean and (con) returns a boxed pair
	(not (cons 1 2))))

(define-test "UTF-8 C string can be boxed as string" (expect "Hello, world!"
	(import (scheme core))
	(import (llambda nfi))
	; See above test for why we need to use the C standard library
	(define strdup (native-function "strdup" (utf8-cstring) utf8-cstring))
	(strdup "Hello, world!")))
