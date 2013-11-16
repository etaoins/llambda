(define-test "datum can be cast to pair" (expect 1
	(import (llambda test-util))
	; This assumes (vector-ref) takes a boxed pair
	(vector-ref (typeless-boxed #(1)) 0)))

(define-test "#false can be unboxed as truthy" (expect #t
	; This assumes (not) takes an unboxed truthy boolean
	(not (car '(#f . #f)))))

(define-test "#false can be unboxed as strict bool" (expect #t
	; This assumes (boolean=?) takes two unboxed booleans
	(boolean=? #f (car '(#f . #f)))))

(define-test "#true can be unboxed as boolean" (expect #f
	(import (llambda test-util))
	; This assumes (not) takes an unboxed truthy
	(not (typeless-boxed #t))))

(define-test "#true can be unboxed as strict bool" (expect #t
	(import (llambda test-util))
	; This assumes (boolean=?) takes two unboxed booleans
	(boolean=? #t (typeless-boxed #t))))

(define-test "empty list can be unboxed as truthy" (expect #f
	(import (llambda test-util))
	; This assumes (not) takes an unboxed truthy
	(not (typeless-boxed '()))))

(define-test "empty list cannot unboxed as strict bool" (expect-failure
	(import (llambda test-util))
	; This assumes (boolean=?) takes two unboxed booleans
	(boolean=? #t (typeless-boxed '()))))

(define-test "exact int can be unboxed as integer" (expect #(#t #t #t)
	(import (llambda test-util))
	; This assumes (make-vector) takes an unboxed exact integer
	(make-vector (typeless-boxed 3) #t)))

(define-test "inexact rational cannot be unboxed as integer" (expect-failure
	(import (llambda test-util))
	; This assumes (make-vector) takes an unboxed exact integer
	(make-vector (typeless-boxed 3.0) #t)))

(define-test "inexact rational can be unboxed as double" (expect 1.0
	(import (scheme inexact))
	(import (llambda test-util))
	; This assumes (cos) takes an unboxed double
	(cos (typeless-boxed 0.0))))

(define-test "inexact rational can be unboxed as float" (expect 10.0
	(import (llambda test-util))
	; Nothing in the stdlib takes float
	(define fabsf (native-function "fabsf" (float) float))
	(fabsf (typeless-boxed -10.0))))

(define-test "exact integer can be unboxed as double" (expect 1.0
	(import (scheme inexact))
	(import (llambda test-util))
	; This assumes (cos) takes an unboxed double
	(cos (typeless-boxed 0))))

(define-test "exact integer can be unboxed as float" (expect 10.0
	(import (llambda test-util))
	; Nothing in the stdlib takes float
	(define fabsf (native-function "fabsf" (float) float))
	(fabsf (typeless-boxed -10))))

(define-test "unboxed i64 can be passed as an unboxed i32" (expect b
	; This assumes (exact) returns an unboxed i64 and (vector-ref) takes an
	; unboxed i32
	(vector-ref #(a b c) (exact 1))))

(define-test "'3' can be unboxed as a character" (expect 3
	(import (llambda test-util))
	; This assumes (digit-value) takes an unboxed Unicode character
	(digit-value (typeless-boxed #\3))))

(define-test "string can be unboxed as a UTF-8 C string" (expect 6
	(import (llambda nfi))
	(import (llambda test-util))
	; Nothing in our stdlib takes UTF-8 C strings because they're binary unsafe
	; and require O(n) importing to determine their length/non-ASCII content.
	; Use strlen from the C standard library for this test
	(define strlen (native-function "strlen" (utf8-cstring) int64))
	(strlen (typeless-boxed "Hello!"))))

(define-test "unboxed int 0 converts to unboxed truthy true" (expect #f
	; This assumes (exact) returns an unboxed integer and (not) takes an unboxed truthy
	(not (exact 0))))

; This seems stupid but it was actually broken at one point
(define-test "unboxed boolean false can be passed to a procedure as truthy" (expect #t
	; This assumes (not) takes an unboxed truthy
	(not (not #t))))

(define-test "unboxed boolean false can be passed to a procedure as strict bool" (expect #f
	; Thie assumes (boolean=? takes two unboxed strict bools
	(boolean=? (not #t) (not #f))))

(define-test "exact integer can be passed to a procedure as float" (expect 10.0
	; Nothing in the stdlib takes float
	(define fabsf (native-function "fabsf" (float) float))
	(fabsf -10)))

; Make sure if we use type analysis to short circuit bool evaluation do it right
; This was also broken at one point
(define-test "types that cannot be boolean evaluate as true" (expect #f
	; This assumes (not) takes an unboxed boolean and (con) returns a boxed pair
	(not (cons 1 2))))

(define-test "UTF-8 C string can be boxed as string" (expect "Hello, world!"
	(import (llambda nfi))
	; See above test for why we need to use the C standard library
	(define strdup (native-function "strdup" (utf8-cstring) utf8-cstring))
	(strdup "Hello, world!")))

; This was broken due to list element being an abstract type
(define-test "datum can be converted to a boxed list element" (expect 4
	(length (append '(1 2) '(3 4)))))
