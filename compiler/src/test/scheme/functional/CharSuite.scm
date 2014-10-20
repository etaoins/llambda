(define-test "'3' is a character" (expect #t
	(char? #\3)))

(define-test "number 3 is not a character" (expect #f
	(char? 3)))

(define-test "digit-value" (expect-success
  (import (scheme char))

	(assert-equal 3 (digit-value #\3))
	(assert-equal 4 (digit-value #\x0664))
	(assert-equal 0 (digit-value #\x0AE6))
	(assert-equal #f (digit-value #\x0EA6))))

(define-test "(char->integer)" (expect-success
  (assert-equal #x41 (char->integer #\x41))
  (assert-equal #x4141 (char->integer #\x4141))
  (assert-equal #x1f3c2 (char->integer #\x1f3c2))

  (assert-equal #x1f3c2 (char->integer (typed-dynamic #\x1f3c2 <char>)))))

(define-test "(integer->char)" (expect-success
	(assert-equal #\x41 (integer->char #x41))
	(assert-equal #\x4141 (integer->char #x4141))
	(assert-equal #\x1f3c2 (integer->char #x1f3c2))

	(assert-equal #\x1f3c2 (integer->char (typed-dynamic #x1f3c2 <exact-integer>)))))

(define-test "(char=?)" (expect-success
  (assert-true  (char=? #\a #\a))
  (assert-false (char=? #\a #\b))

  (assert-true  (char=? #\a #\a #\a))
  (assert-false (char=? #\a #\a #\b))))

(define-test "(char<?)" (expect-success
  (assert-true  (char<? #\a #\b))
  (assert-false (char<? #\a #\a))
  (assert-false (char<? #\b #\a))

  (assert-true  (char<? #\a #\b #\c))
  (assert-false (char<? #\b #\b #\b))
  (assert-false (char<? #\c #\b #\a))))

(define-test "(char>?)" (expect-success
  (assert-false (char>? #\a #\b))
  (assert-false (char>? #\a #\a))
  (assert-true  (char>? #\b #\a))

  (assert-false (char>? #\a #\b #\c))
  (assert-false (char>? #\b #\b #\b))
  (assert-true  (char>? #\c #\b #\a))))

(define-test "(char<=?)" (expect-success
  (assert-true  (char<=? #\a #\b))
  (assert-true  (char<=? #\a #\a))
  (assert-false (char<=? #\b #\a))

  (assert-true  (char<=? #\a #\b #\c))
  (assert-true  (char<=? #\b #\b #\b))
  (assert-false (char<=? #\c #\b #\a))))

(define-test "(char>=?)" (expect-success
  (assert-false (char>=? #\a #\b))
  (assert-true  (char>=? #\a #\a))
  (assert-true  (char>=? #\b #\a))

  (assert-false (char>=? #\a #\b #\c))
  (assert-true  (char>=? #\b #\b #\b))
  (assert-true  (char>=? #\c #\b #\a))))
