(define-test "exact integer is number" (expect #t
	(import (scheme core))
	(number? 4)))

(define-test "inexact rational is number" (expect #t
	(import (scheme core))
	(number? -5.0)))

(define-test "empty list is not number" (expect #f
	(import (scheme core))
	(number? '())))

(define-test "exact integer is real" (expect #t
	(import (scheme core))
	(real? 4)))

(define-test "inexact rational is real" (expect #t
	(import (scheme core))
	(real? -5.0)))

(define-test "empty list is not real" (expect #f
	(import (scheme core))
	(real? '())))

(define-test "exact integer is rational" (expect #t
	(import (scheme core))
	(rational? 4)))

(define-test "inexact rational is rational" (expect #t
	(import (scheme core))
	(rational? -5.0)))

(define-test "empty list is not rational" (expect #f
	(import (scheme core))
	(rational? '())))

(define-test "3.0 is not exact" (expect #f
	(import (scheme core))
	(exact? 3.0)))

(define-test "3. is not inexact" (expect #f
	(import (scheme core))
	(inexact? 3.)))

(define-test "32 is an exact integer" (expect #t
	(import (scheme core))
	(exact-integer? 32)))

(define-test "32.0 is not an exact integer" (expect #f
	(import (scheme core))
	(exact-integer? 32.0)))

(define-test "Exact -32.0 is -32" (expect -32
	(import (scheme core))
	(exact -32.0)))

(define-test "Exact 64 is 64" (expect 64
	(import (scheme core))
	(exact 64)))

(define-test "Exact 112.5 fails" (expect-failure
	(import (scheme core))
	(exact 112.5)))

(define-test "Inexact 567 is 567.0" (expect 567.0
	(import (scheme core))
	(inexact 567)))

(define-test "Inexact -3289.5 is -3289.5" (expect -3289.5
	(import (scheme core))
	(inexact -3289.5)))

; This can't be exactly represented by a double
(define-test "Inexact 9007199254740993 fails" (expect-failure
	(import (scheme core))
	(inexact 9007199254740993)))

; Super ghetto but anything else depends too much on floating point
; representations
(define-test "inexact sin 0 is 0" (expect 0.0
	(import (scheme core))
	(sin 0.0)))

(define-test "inexact cos 0 is 1" (expect 1.0
	(import (scheme core))
	(cos 0.0)))

(define-test "inexact tan 0 is 0" (expect 0.0
	(import (scheme core))
	(tan 0.0)))

(define-test "exact sin 0 is 0" (expect 0.0
	(import (scheme core))
	(sin 0)))

(define-test "exact cos 0 is 1" (expect 1.0
	(import (scheme core))
	(cos 0)))

(define-test "exact tan 0 is 0" (expect 0.0
	(import (scheme core))
	(tan 0)))
