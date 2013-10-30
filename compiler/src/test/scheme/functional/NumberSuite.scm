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
	(import (scheme core) (scheme inexact))
	(sin 0.0)))

(define-test "inexact cos 0 is 1" (expect 1.0
	(import (scheme core) (scheme inexact))
	(cos 0.0)))

(define-test "inexact tan 0 is 0" (expect 0.0
	(import (scheme core) (scheme inexact))
	(tan 0.0)))

(define-test "exact sin 0 is 0" (expect 0.0
	(import (scheme core) (scheme inexact))
	(sin 0)))

(define-test "exact cos 0 is 1" (expect 1.0
	(import (scheme core) (scheme inexact))
	(cos 0)))

(define-test "exact tan 0 is 0" (expect 0.0
	(import (scheme core) (scheme inexact))
	(tan 0)))

(define-test "adding no numbers is exact 0" (expect 0
	(import (scheme core))
	(+)))

(define-test "adding single exact number is that exact number" (expect 12
	(import (scheme core))
	(+ 12)))

(define-test "adding single inexact number is that inexact number" (expect -450.5
	(import (scheme core))
	(+ -450.5)))

(define-test "adding single string fails" (expect-failure
	(import (scheme core))
	(+ "Hello!")))

(define-test "adding three exact numbers is their exact sum" (expect -435065
	(import (scheme core))
	(+ 70 -1024589 589454)))

(define-test "adding three inexact numbers is their inexact sum" (expect 300.0
	(import (scheme core))
	(+ 100.5 -0.5 200.0)))

(define-test "adding two exact numbers and one inexact number their inexact sum" (expect 300.0
	(import (scheme core))
	(+ 100.5 -0.5 200)))

(define-test "multiplying no numbers is exact 1" (expect 1
	(import (scheme core))
	(*)))

(define-test "multiplying single exact number is that exact number" (expect 12
	(import (scheme core))
	(* 12)))

(define-test "multiplying single inexact number is that inexact number" (expect -450.5
	(import (scheme core))
	(* -450.5)))

(define-test "multiplying single string fails" (expect-failure
	(import (scheme core))
	(* "Hello!")))

(define-test "multiplying three exact numbers is their exact product" (expect -499332738025
	(import (scheme core))
	(* 4135 -3547 34045)))

(define-test "multiplying three inexact numbers is their inexact product" (expect -10050.0
	(import (scheme core))
	(* 100.5 -0.5 200.0)))

(define-test "multiplying two exact numbers and one inexact number their inexact product" (expect 10050.0
	(import (scheme core))
	(* 100.5 0.5 200)))

(define-test "subtracting no numbers fails" (expect-failure
	(import (scheme core))
	(-)))

(define-test "subtracting single exact number is the exact inverse of that number" (expect -12
	(import (scheme core))
	(- 12)))

(define-test "subtracting single inexact number is the inexact inverse of that number" (expect 450.5
	(import (scheme core))
	(- -450.5)))

(define-test "subtracting single string fails" (expect-failure
	(import (scheme core))
	(- "Hello!")))

(define-test "subtracting three exact numbers is their exact difference" (expect -26363
	(import (scheme core))
	(- 4135 -3547 34045)))

(define-test "subtracting three inexact numbers is their inexact difference" (expect -99.0
	(import (scheme core))
	(- 100.5 -0.5 200.0)))

(define-test "subtracting two exact numbers and one inexact number their inexact difference" (expect -100.0
	(import (scheme core))
	(- 100.5 0.5 200)))

(define-test "dividing no numbers fails" (expect-failure
	(import (scheme core))
	(-)))

(define-test "dividing single exact number is the inexact reciprocal of that exact number" (expect 0.125
	(import (scheme core))
	(/ 8)))

(define-test "dividing single inexact number is the reciprocal of that inexact number" (expect -4.0
	(import (scheme core))
	(/ -0.25)))

(define-test "dividing single string fails" (expect-failure
	(import (scheme core))
	(/ "Hello!")))

(define-test "dividing three exact numbers is inexact" (expect 0.15
	(import (scheme core))
	(/ 3 4 5)))

(define-test "dividing three inexact numbers is inexact" (expect 64.0
	(import (scheme core))
	(/ 128.0 0.25 8)))

(define-test "dividing two exact numbers and one inexact number is inexact" (expect -64.0
	(import (scheme core))
	(/ 128.0 -0.25 8)))
