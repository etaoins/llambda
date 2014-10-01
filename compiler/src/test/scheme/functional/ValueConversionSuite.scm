(define-test "datum can be cast to pair" (expect 1
	; This assumes (vector-ref) takes a pair
	(vector-ref (typeless-cell #(1)) 0)))

(define-test "#false can be unboxed as truthy" (expect #t
	; This assumes (not) takes a truthy boolean
	(not dynamic-false)))

(define-test "#false can be unboxed as bool" (expect #t
	; This assumes (boolean=?) takes two native booleans
	(boolean=? #f dynamic-false)))

(define-test "#true can be unboxed as boolean" (expect #f
	; This assumes (not) takes a truthy
	(not (typeless-cell #t))))

(define-test "#true can be unboxed as bool" (expect #t
	; This assumes (boolean=?) takes two native booleans
	(boolean=? #t (typeless-cell #t))))

(define-test "empty list can be unboxed as truthy" (expect #f
	; This assumes (not) takes a truthy
	(not (typeless-cell '()))))

(define-test "empty list cannot unboxed as bool" (expect-failure
	; This assumes (boolean=?) takes two native booleans
	(boolean=? #t (typeless-cell '()))))

(define-test "exact int can be unboxed as integer" (expect #(#t #t #t)
	; This assumes (make-vector) takes an native exact integer
	(make-vector (typeless-cell 3) #t)))

(define-test "inexact rational cannot be unboxed as integer" (expect-failure
	; This assumes (make-vector) takes an native exact integer
	(make-vector (typeless-cell 3.0) #t)))

(define-test "inexact rational can be unboxed as double" (expect 1.0
	(define fabs (native-function "fabs" (<native-double>) -> <native-double>))
	(fabs (typeless-cell -1.0))))

(define-test "inexact rational can be unboxed as float" (expect 10.0
	(define fabsf (native-function "fabsf" (<native-float>) -> <native-float>))
	(fabsf (typeless-cell -10.0))))

(define-test "exact integer cannot be unboxed as double" (expect-failure
	(define fabs (native-function "fabs" (<native-double>) -> <native-double>))
	(fabs (typeless-cell 0))))

(define-test "exact integer cannot be unboxed as float" (expect-failure
	(define fabsf (native-function "fabsf" (<native-float>) -> <native-float>))
	(fabsf (typeless-cell -10))))

(define-test "native i64 can be passed as an native i32" (expect b
	; This assumes (exact) returns an native i64 and (vector-ref) takes an
	; native i32
	(vector-ref #(a b c) (exact 1))))

(define-test "native i64 cannot be boxed as an inexact rational" (expect-failure
	(define inexact->inexact (world-function "lliby_inexact" (<flonum>) -> <native-double>))
	; This assumes (exact) returns an native i64
	(inexact->inexact (exact -53))))

(define-test "constant exact integer cannot be boxed as an inexact rational" (expect-failure
	(define inexact->inexact (world-function "lliby_inexact" (<flonum>) -> <native-double>))
	; This assumes (exact) returns an native i64
	(inexact->inexact -53)))

(define-test "'3' can be unboxed as a character" (expect 3
	; This assumes (digit-value) takes an native Unicode character
	(digit-value (typeless-cell #\3))))

(define-test "native int 0 converts to unboxed truthy true" (expect #f
	; This assumes (exact) returns an native integer and (not) takes a truthy
	(not (exact 0))))

; This seems stupid but it was actually broken at one point
(define-test "native boolean false can be passed to a procedure as truthy" (expect #t
	; This assumes (not) takes a truthy
	(not (not #t))))

(define-test "native boolean false can be passed to a procedure as bool" (expect #f
	; Thie assumes (boolean=? takes two bools)
	(boolean=? (not #t) (not #f))))

(define-test "exact integer can be passed to a procedure as float" (expect-failure
	; Nothing in the stdlib takes float
	(define fabsf (native-function "fabsf" (<native-float>) -> <native-float>))
	(fabsf -10)))

; Make sure if we use type analysis to short circuit bool evaluation do it right
; This was also broken at one point
(define-test "types that cannot be boolean evaluate as true" (expect #f
	; This assumes (not) takes an native boolean and (con) returns a pair
	(not (cons 1 2))))

; This was broken due to list element being an abstract type
(define-test "datum can be converted to a list element" (expect 4
	(length (append '(1 2) '(3 4)))))

(define-test "(make-bytevector) only accepts positive constant lengths" (expect-compile-failure
  (define native-make-bytevector (world-function "lliby_make_bytevector" (<native-uint32> <native-uint8>) -> <bytevector>))
  (native-make-bytevector -1 0)))

(define-test "(make-bytevector) only accepts 32bit constant lengths" (expect-compile-failure
  (define native-make-bytevector (world-function "lliby_make_bytevector" (<native-uint32> <native-uint8>) -> <bytevector>))
  (native-make-bytevector 4294967296 0)))

(define-test "(make-bytevector) only accepts positive constant fill values" (expect-compile-failure
  (define native-make-bytevector (world-function "lliby_make_bytevector" (<native-uint32> <native-uint8>) -> <bytevector>))
  (native-make-bytevector 1 -1)))

(define-test "(make-bytevector) only accepts 8bit constant fill values" (expect-compile-failure
  (define native-make-bytevector (world-function "lliby_make_bytevector" (<native-uint32> <native-int8>) -> <bytevector>))
  (native-make-bytevector 1 256)))
