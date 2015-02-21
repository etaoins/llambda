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

(define-test "(char-alphabetic?)" (expect-success
  (import (scheme char))

  (assert-false (char-alphabetic? #\space))

  (assert-true  (char-alphabetic? #\a))
  (assert-true  (char-alphabetic? #\Z))
  ; Greek captital letter yot (Unicode 7)
  (assert-true  (char-alphabetic? #\x037F))

  (assert-false (char-alphabetic? #\1))
  ; Arabic-indic 0
  (assert-false (char-alphabetic? #\x0660))

  ; Han root, origin, source, basis
  (assert-true (char-alphabetic? #\本))

  ; Snowboarder
  (assert-false (char-alphabetic? #\x1f3c2))))

(define-test "(char-numeric?)" (expect-success
  (import (scheme char))

  (assert-false (char-numeric? #\space))
  (assert-false (char-numeric? #\a))
  (assert-false (char-numeric? #\Z))
  (assert-false (char-numeric? #\x037F))
  (assert-true  (char-numeric? #\1))
  (assert-true  (char-numeric? #\x0660))
  (assert-false (char-numeric? #\本))
  (assert-false (char-numeric? #\x1f3c2))))

(define-test "(char-whitespace?)" (expect-success
  (import (scheme char))

  (assert-true  (char-whitespace? #\space))
  (assert-false (char-whitespace? #\a))
  (assert-false (char-whitespace? #\Z))
  (assert-false (char-whitespace? #\x037F))
  (assert-false (char-whitespace? #\1))
  (assert-false (char-whitespace? #\x0660))
  (assert-false (char-whitespace? #\本))
  (assert-false (char-whitespace? #\x1f3c2))))

(define-test "(char-upper-case?)" (expect-success
  (import (scheme char))

  (assert-false (char-upper-case? #\space))
  (assert-false (char-upper-case? #\a))
  (assert-true  (char-upper-case? #\Z))
  (assert-true  (char-upper-case? #\x037F))
  (assert-false (char-upper-case? #\1))
  (assert-false (char-upper-case? #\x0660))
  (assert-false (char-upper-case? #\本))
  (assert-false (char-upper-case? #\x1f3c2))))

(define-test "(char-lower-case?)" (expect-success
  (import (scheme char))

  (assert-false (char-lower-case? #\space))
  (assert-true  (char-lower-case? #\a))
  (assert-false (char-lower-case? #\Z))
  (assert-false (char-lower-case? #\x037F))
  (assert-false (char-lower-case? #\1))
  (assert-false (char-lower-case? #\x0660))
  (assert-false (char-lower-case? #\本))
  (assert-false (char-lower-case? #\x1f3c2))))

(define-test "(char-upcase)" (expect-success
  (import (scheme char))

  (assert-equal #\A (char-upcase #\a))
  (assert-equal #\Z (char-upcase #\Z))
  (assert-equal #\Σ (char-upcase #\σ))
  (assert-equal #\Σ (char-upcase #\Σ))
  (assert-equal #\1 (char-upcase #\1))

  (assert-equal #\S (char-upcase #\x017F))))

(define-test "(char-downcase)" (expect-success
  (import (scheme char))

  (assert-equal #\a (char-downcase #\a))
  (assert-equal #\z (char-downcase #\Z))
  (assert-equal #\σ (char-downcase #\σ))
  (assert-equal #\σ (char-downcase #\Σ))
  (assert-equal #\1 (char-downcase #\1))

  (assert-equal #\x017F (char-downcase #\x017F))))

(define-test "(char-foldcase)" (expect-success
  (import (scheme char))

  (assert-equal #\a (char-foldcase #\a))
  (assert-equal #\z (char-foldcase #\Z))
  (assert-equal #\σ (char-foldcase #\σ))
  (assert-equal #\σ (char-foldcase #\Σ))
  (assert-equal #\1 (char-foldcase #\1))

  (assert-equal #\s (char-foldcase #\x017F))))

(define-test "static (char->integer)" (expect-static-success
  (assert-equal #x41 (char->integer #\x41))
  (assert-equal #x4141 (char->integer #\x4141))
  (assert-equal #x1f3c2 (char->integer #\x1f3c2))))

(define-test "dynamic (char->integer)" (expect-success
  (assert-equal #x1f3c2 (char->integer (typed-dynamic #\x1f3c2 <char>)))))

(define-test "static (integer->char)" (expect-static-success
	(assert-equal #\x41 (integer->char #x41))
	(assert-equal #\x4141 (integer->char #x4141))
	(assert-equal #\x1f3c2 (integer->char #x1f3c2))))

(define-test "dynamic (integer->char)" (expect-success
	(assert-equal #\x1f3c2 (integer->char (typed-dynamic #x1f3c2 <exact-integer>)))))

(define-test "(integer->char) with negative code point fails" (expect-error range-error?
  (integer->char -1)))

(define-test "(integer->char) with out of range code point fails" (expect-error range-error?
  (integer->char #x110000)))

(define-test "(char=?)" (expect-static-success
  (assert-false (char=? #\a #\b))
  (assert-true  (char=? #\a #\a))
  (assert-false (char=? #\a #\A))
  (assert-false (char=? #\b #\a))

  (assert-true  (char=? #\a #\a #\a))
  (assert-false (char=? #\a #\a #\b))))

(define-test "(char<?)" (expect-static-success
  (assert-true  (char<? #\a #\b))
  (assert-false (char<? #\a #\a))
  (assert-false (char<? #\a #\A))
  (assert-false (char<? #\b #\a))

  (assert-true  (char<? #\a #\b #\c))
  (assert-false (char<? #\b #\b #\b))
  (assert-false (char<? #\c #\b #\a))))

(define-test "(char>?)" (expect-static-success
  (assert-false (char>? #\a #\b))
  (assert-false (char>? #\a #\a))
  (assert-true  (char>? #\a #\A))
  (assert-true  (char>? #\b #\a))

  (assert-false (char>? #\a #\b #\c))
  (assert-false (char>? #\b #\b #\b))
  (assert-true  (char>? #\c #\b #\a))))

(define-test "(char<=?)" (expect-static-success
  (assert-true  (char<=? #\a #\b))
  (assert-true  (char<=? #\a #\a))
  (assert-false (char<=? #\a #\A))
  (assert-false (char<=? #\b #\a))

  (assert-true  (char<=? #\a #\b #\c))
  (assert-true  (char<=? #\b #\b #\b))
  (assert-false (char<=? #\c #\b #\a))))

(define-test "(char>=?)" (expect-static-success
  (assert-false (char>=? #\a #\b))
  (assert-true  (char>=? #\a #\a))
  (assert-true  (char>=? #\a #\A))
  (assert-true  (char>=? #\b #\a))

  (assert-false (char>=? #\a #\b #\c))
  (assert-true  (char>=? #\b #\b #\b))
  (assert-true  (char>=? #\c #\b #\a))))

(define-test "(char-ci=?)" (expect-success
  (import (scheme char))

  (assert-false (char-ci=? #\a #\b))
  (assert-true  (char-ci=? #\a #\a))
  (assert-true  (char-ci=? #\a #\A))
  (assert-false (char-ci=? #\b #\a))

  (assert-true  (char-ci=? #\a #\a #\a))
  (assert-false (char-ci=? #\a #\a #\b))))

(define-test "(char-ci<?)" (expect-success
  (import (scheme char))

  (assert-true  (char-ci<? #\a #\b))
  (assert-false (char-ci<? #\a #\a))
  (assert-false (char-ci<? #\a #\A))
  (assert-false (char-ci<? #\b #\a))

  (assert-true  (char-ci<? #\a #\b #\c))
  (assert-false (char-ci<? #\b #\b #\b))
  (assert-false (char-ci<? #\c #\b #\a))))

(define-test "(char-ci>?)" (expect-success
  (import (scheme char))

  (assert-false (char-ci>? #\a #\b))
  (assert-false (char-ci>? #\a #\a))
  (assert-false (char-ci>? #\a #\A))
  (assert-true  (char-ci>? #\b #\a))

  (assert-false (char-ci>? #\a #\b #\c))
  (assert-false (char-ci>? #\b #\b #\b))
  (assert-true  (char-ci>? #\c #\b #\a))))

(define-test "(char-ci<=?)" (expect-success
  (import (scheme char))

  (assert-true  (char-ci<=? #\a #\b))
  (assert-true  (char-ci<=? #\a #\a))
  (assert-true  (char-ci<=? #\a #\A))
  (assert-false (char-ci<=? #\b #\a))

  (assert-true  (char-ci<=? #\a #\b #\c))
  (assert-true  (char-ci<=? #\b #\b #\b))
  (assert-false (char-ci<=? #\c #\b #\a))))

(define-test "(char-ci>=?)" (expect-success
  (import (scheme char))

  (assert-false (char-ci>=? #\a #\b))
  (assert-true  (char-ci>=? #\a #\a))
  (assert-true  (char-ci>=? #\a #\A))
  (assert-true  (char-ci>=? #\b #\a))

  (assert-false (char-ci>=? #\a #\b #\c))
  (assert-true  (char-ci>=? #\b #\b #\b))
  (assert-true  (char-ci>=? #\c #\b #\a))))
