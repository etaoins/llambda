(define-test "exact integer is number" (expect #t
	(number? 4)))

(define-test "inexact rational is number" (expect #t
	(number? -5.0)))

(define-test "empty list is not number" (expect #f
	(number? '())))

(define-test "exact integer is real" (expect #t
	(real? 4)))

(define-test "inexact rational is real" (expect #t
	(real? -5.0)))

(define-test "empty list is not real" (expect #f
	(real? '())))

(define-test "exact integer is rational" (expect #t
	(rational? 4)))

(define-test "inexact rational is rational" (expect #t
	(rational? -5.0)))

(define-test "empty list is not rational" (expect #f
	(rational? '())))

(define-test "exact integer is complex" (expect #t
	(complex? 4)))

(define-test "inexact complex is complex" (expect #t
	(complex? -5.0)))

(define-test "empty list is not complex" (expect #f
	(complex? '())))

(define-test "3.0 is not exact" (expect #f
	(exact? 3.0)))

(define-test "3. is not inexact" (expect #f
	(inexact? 3.)))

(define-test "3 is finite" (expect #t
	(finite? 3)))

(define-test "4.5 is finite" (expect #t
	(finite? 4.5)))

(define-test "+inf.0 is not finite" (expect #f
	(finite? +inf.0)))

(define-test "+nan.0 is not finite" (expect #f
	(finite? +nan.0)))

(define-test "3 is not infinite" (expect #f
	(infinite? 3)))

(define-test "4.5 is not infinite" (expect #f
	(infinite? 4.5)))

(define-test "+inf.0 is infinite" (expect #t
	(infinite? +inf.0)))

(define-test "+nan.0 is not infinite" (expect #f
	(infinite? +nan.0)))

(define-test "3 is not NaN" (expect #f
	(nan? 3)))

(define-test "4.5 is not NaN" (expect #f
	(nan? 4.5)))

(define-test "+inf.0 is not NaN" (expect #f
	(nan? +inf.0)))

(define-test "+nan.0 is NaN" (expect #t
	(nan? +nan.0)))

(define-test "32 is an exact integer" (expect #t
	(exact-integer? 32)))

(define-test "32.0 is not an exact integer" (expect #f
	(exact-integer? 32.0)))

(define-test "Exact -32.0 is -32" (expect -32
	(exact -32.0)))

(define-test "Exact 64 is 64" (expect 64
	(exact 64)))

(define-test "Exact 112.5 fails" (expect-failure
	(exact 112.5)))

(define-test "Inexact 567 is 567.0" (expect 567.0
	(inexact 567)))

(define-test "Inexact -3289.5 is -3289.5" (expect -3289.5
	(inexact -3289.5)))

; This can't be exactly represented by a double
(define-test "Inexact 9007199254740993 fails" (expect-failure
	(inexact 9007199254740993)))

; Super ghetto but anything else depends too much on floating point
; representations
(define-test "inexact sin 0 is 0" (expect 0.0
	(import (scheme inexact))
	(sin 0.0)))

(define-test "inexact cos 0 is 1" (expect 1.0
	(import (scheme inexact))
	(cos 0.0)))

(define-test "inexact tan 0 is 0" (expect 0.0
	(import (scheme inexact))
	(tan 0.0)))

(define-test "exact sin 0 is 0" (expect 0.0
	(import (scheme inexact))
	(sin 0)))

(define-test "exact cos 0 is 1" (expect 1.0
	(import (scheme inexact))
	(cos 0)))

(define-test "exact tan 0 is 0" (expect 0.0
	(import (scheme inexact))
	(tan 0)))

(define-test "adding no numbers is exact 0" (expect 0
	(+)))

(define-test "adding single exact number is that exact number" (expect 12
	(+ 12)))

(define-test "adding single inexact number is that inexact number" (expect -450.5
	(+ -450.5)))

(define-test "adding single string fails" (expect-failure
	(+ "Hello!")))

(define-test "adding three exact numbers is their exact sum" (expect -435065
	(+ 70 -1024589 589454)))

(define-test "adding three inexact numbers is their inexact sum" (expect 300.0
	(+ 100.5 -0.5 200.0)))

(define-test "adding two exact numbers and one inexact number their inexact sum" (expect 300.0
	(+ 100.5 -0.5 200)))

(define-test "multiplying no numbers is exact 1" (expect 1
	(*)))

(define-test "multiplying single exact number is that exact number" (expect 12
	(* 12)))

(define-test "multiplying single inexact number is that inexact number" (expect -450.5
	(* -450.5)))

(define-test "multiplying single string fails" (expect-failure
	(* "Hello!")))

(define-test "multiplying three exact numbers is their exact product" (expect -499332738025
	(* 4135 -3547 34045)))

(define-test "multiplying three inexact numbers is their inexact product" (expect -10050.0
	(* 100.5 -0.5 200.0)))

(define-test "multiplying two exact numbers and one inexact number their inexact product" (expect 10050.0
	(* 100.5 0.5 200)))

(define-test "subtracting no numbers fails" (expect-failure
	(-)))

(define-test "subtracting single exact number is the exact inverse of that number" (expect -12
	(- 12)))

(define-test "subtracting single inexact number is the inexact inverse of that number" (expect 450.5
	(- -450.5)))

(define-test "subtracting single string fails" (expect-failure
	(- "Hello!")))

(define-test "subtracting three exact numbers is their exact difference" (expect -26363
	(- 4135 -3547 34045)))

(define-test "subtracting three inexact numbers is their inexact difference" (expect -99.0
	(- 100.5 -0.5 200.0)))

(define-test "subtracting two exact numbers and one inexact number their inexact difference" (expect -100.0
	(- 100.5 0.5 200)))

(define-test "dividing no numbers fails" (expect-failure
	(-)))

(define-test "dividing single exact number is the inexact reciprocal of that exact number" (expect 0.125
	(/ 8)))

(define-test "dividing single inexact number is the reciprocal of that inexact number" (expect -4.0
	(/ -0.25)))

(define-test "dividing single string fails" (expect-failure
	(/ "Hello!")))

(define-test "dividing three exact numbers is inexact" (expect 0.15
	(/ 3 4 5)))

(define-test "dividing three inexact numbers is inexact" (expect 64.0
	(/ 128.0 0.25 8)))

(define-test "dividing two exact numbers and one inexact number is inexact" (expect -64.0
	(/ 128.0 -0.25 8)))

(define-test "equality of two equal numbers is true" (expect #t
	(= 4.0 4)))

(define-test "equality of 0.0 and -0.0 is true" (expect #t
	(= 0.0 -0.0)))

(define-test "equality of three equal numbers is true" (expect #t
	(= 4.0 4 4.0)))

(define-test "equality of two numbers and boolean false is an error" (expect-failure
	(= 4.0 4 #f)))

(define-test "equality of two inequal numbers is false" (expect #f
	(= 4.0 5.6)))

(define-test "equality of three inequal numbers is false" (expect #f
	(= 4.0 4 5.6)))

(define-test "lesser than of two equal numbers is false" (expect #f
	(< 4.0 4)))

(define-test "lesser than of 0.0 and -0.0 is false" (expect #f
	(< -0.0 0.0)))

(define-test "lesser than of three equal numbers is false" (expect #f
	(< 4.0 4 4.0)))

(define-test "lesser than of two decreasing numbers is false" (expect #f
	(< 5.6 4.0)))

(define-test "lesser than of three decreasing numbers is false" (expect #f
	(< 5.6 0 -4.5)))

(define-test "lesser than of two increasing numbers is true" (expect #t
	(< 4.0 5.6)))

(define-test "lesser than of three increasing numbers is true" (expect #t
	(< 4.0 4.5 5.6)))

(define-test "greater than of two equal numbers is false" (expect #f
	(> 4.0 4)))

(define-test "greater than of 0.0 and -0.0 is false" (expect #f
	(> -0.0 0.0)))

(define-test "greater than of three equal numbers is false" (expect #f
	(> 4.0 4 4.0)))

(define-test "greater than of two decreasing numbers is true" (expect #t
	(> 5.6 4.0)))

(define-test "greater than of three decreasing numbers is true" (expect #t
	(> 5.6 0 -4.5)))

(define-test "greater than of two increasing numbers is false" (expect #f
	(> 4.0 5.6)))

(define-test "greater than of three increasing numbers is false" (expect #f
	(> 4.0 4.5 5.6)))

(define-test "lesser than or equal of two equal numbers is true" (expect #t
	(<= 4.0 4)))

(define-test "lesser than or equal of 0.0 and -0.0 is true" (expect #t
	(<= -0.0 0.0)))

(define-test "lesser than or equal of three equal numbers is true" (expect #t
	(<= 4.0 4 4.0)))

(define-test "lesser than or equal of two decreasing numbers is false" (expect #f
	(<= 5.6 4.0)))

(define-test "lesser than or equal of three decreasing numbers is false" (expect #f
	(<= 5.6 0 -4.5)))

(define-test "lesser than or equal of two increasing numbers is true" (expect #t
	(<= 4.0 5.6)))

(define-test "lesser than or equal of three increasing numbers is true" (expect #t
	(<= 4.0 4.5 5.6)))

(define-test "greater than or equal of two equal numbers is true" (expect #t
	(>= 4.0 4)))

(define-test "greater than or equal of 0.0 and -0.0 is true" (expect #t
	(>= -0.0 0.0)))

(define-test "greater than or equal of three equal numbers is true" (expect #t
	(>= 4.0 4 4.0)))

(define-test "greater than or equal of two decreasing numbers is true" (expect #t
	(>= 5.6 4.0)))

(define-test "greater than or equal of three decreasing numbers is true" (expect #t
	(>= 5.6 0 -4.5)))

(define-test "greater than or equal of two increasing numbers is false" (expect #f
	(>= 4.0 5.6)))

(define-test "greater than or equal of three increasing numbers is false" (expect #f
	(>= 4.0 4.5 5.6)))

