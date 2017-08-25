; This test assumes the inline -> heap transition happens after 28 bytes
(define-test "symbol constant is a symbol" (expect #t
  (symbol? 'test)))

(define-test "symbol constant of maximum inline size" (expect this-symbol-is-28-bytes-long
  'this-symbol-is-28-bytes-long))

(define-test "empty list is not a symbol" (expect #f
  (string? '())))

(define-test "static (symbol=?)" (expect-static-success
  (assert-true (symbol=? 'alpha 'alpha))
  (assert-false (symbol=? 'alpha 'beta))
  (assert-false (symbol=? 'alpha 'alpha 'beta))

  ; Inline symbols
  (assert-true (symbol=? 'alpha 'alpha 'alpha))

  ; Heap symbols
  (assert-true (symbol=? 'excessively-long-test-symbol 'excessively-long-test-symbol 'excessively-long-test-symbol))))

(define-test "dynamic (symbol=?)" (expect-success
  (import (llambda typed))

  (define alpha-or-beta (typed-dynamic 'alpha (U 'alpha 'beta)))
  (define gamma-or-beta (typed-dynamic 'gamma (U 'beta 'gamma)))
  (define untyped-gamma (typed-dynamic 'gamma <any>))

  ; Ensure all the comparisons of unsuccessful comparisons work
  (assert-false (symbol=? alpha-or-beta gamma-or-beta))
  (assert-false (symbol=? alpha-or-beta untyped-gamma))
  (assert-false (symbol=? alpha-or-beta 'gamma))
  (assert-false (symbol=? alpha-or-beta 'alpha gamma-or-beta))

  ; Ensure all of the combinations of successful comparisons work
  (assert-true (symbol=? alpha-or-beta 'alpha))
  (assert-true (symbol=? gamma-or-beta 'gamma))
  (assert-true (symbol=? untyped-gamma 'gamma))
  (assert-true (symbol=? untyped-gamma gamma-or-beta))
  (assert-true (symbol=? untyped-gamma gamma-or-beta 'gamma))))

(define-test "(symbol=?) with one arg fails" (expect-compile-error arity-error?
  (symbol=? 'test)))

(define-test "(symbol=?) with non-symbol fails" (expect-compile-error type-error?
  (symbol=? 'test 'test "test")))

(define-test "(symbol->string)" (expect-static-success
  ; Inline symbol
  (assert-equal "flying-cat" (symbol->string 'flying-cat))
  ; Heap symbol
  (assert-equal "canadian-tiger-swallowtail-butterfly" (symbol->string 'canadian-tiger-swallowtail-butterfly))

  (define non-bmp-string (symbol->string '|Hell🏂!|))
  (assert-equal "Hell🏂!" non-bmp-string)
  (assert-equal 6 (string-length non-bmp-string))))

(define-test "(string->symbol)" (expect-static-success
  ; Inline symbol; inline string
  (assert-equal 'mISSISSIppi (string->symbol "mISSISSIppi"))
  ; Heap symbol; heap string
  (assert-equal 'Llanfairpwllgwyngyllgogerychwyrndrobwllllantysiliogogogoch
                (string->symbol "Llanfairpwllgwyngyllgogerychwyrndrobwllllantysiliogogogoch"))))

(define-test "(string->symbol (symbol->string))" (expect-static-success
  ; Inline symbol; inline string
  (assert-equal 'LollyPop (string->symbol (symbol->string 'LollyPop)))
  ; Heap symbol; heap string
  (assert-equal '|Fun Sized Reese's Peanut Butter Cup| (string->symbol (symbol->string '|Fun Sized Reese's Peanut Butter Cup|)))
  ; Non-BMP symbol
  (assert-equal '|Hell🏂!| (string->symbol (symbol->string '|Hell🏂!|)))))
