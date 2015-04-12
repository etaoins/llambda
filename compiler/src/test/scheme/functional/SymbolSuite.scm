; This test assumes the inline -> heap transition happens after 12 bytes
(define-test "symbol constant is a symbol" (expect #t
  (symbol? 'test)))

(define-test "symbol constant of maximum inline size" (expect crash-length
  'crash-length))

(define-test "empty list is not a symbol" (expect #f
  (string? '())))

(define-test "(symbol=?)" (expect-success
  (assert-true (symbol=? 'test 'test))
  (assert-false (symbol=? 'test 'nottest))
  (assert-false (symbol=? 'test 'test 'nottest))

  ; Inline symbols
  (assert-true (symbol=? 'test 'test 'test))

  ; Heap symbols
  (assert-true (symbol=? 'very-long-test-symbol 'very-long-test-symbol 'very-long-test-symbol))))

(define-test "(symbol=?) with one arg fails" (expect-compile-error arity-error?
  (symbol=? 'test)))

(define-test "(symbol=?) with non-symbol fails" (expect-compile-error type-error?
  (symbol=? 'test 'test "test")))

(define-test "(symbol->string)" (expect-static-success
  ; Inline symbol
  (assert-equal "flying-cat" (symbol->string 'flying-cat))
  ; Heap symbol
  (assert-equal "flying-hippopotamus" (symbol->string 'flying-hippopotamus))

  (define non-bmp-string (symbol->string '|Hell🏂!|))
  (assert-equal "Hell🏂!" non-bmp-string)
  (assert-equal 6 (string-length non-bmp-string))))

(define-test "(string->symbol)" (expect-static-success
  ; Inline symbol; inline string
  (assert-equal 'mISSISSIppi (string->symbol "mISSISSIppi"))
  ; Inline symbol; heap string
  (assert-equal 'MassaCHUsetts (string->symbol "MassaCHUsetts"))
  ; Heap symbol; heap string
  (assert-equal 'Yamagawaokachiyogamizu (string->symbol "Yamagawaokachiyogamizu"))))

(define-test "(string->symbol (symbol->string))" (expect-static-success
  ; Inline symbol; inline string
  (assert-equal 'LollyPop (string->symbol (symbol->string 'LollyPop)))
  ; Inline symbol; heap string
  (assert-equal 'SourPatchKids (string->symbol (symbol->string 'SourPatchKids)))
  ; Heap symbol; heap string
  (assert-equal '|Reese's Peanut Butter Cup| (string->symbol (symbol->string '|Reese's Peanut Butter Cup|)))
  ; Non-BMP symbol
  (assert-equal '|Hell🏂!| (string->symbol (symbol->string '|Hell🏂!|)))))
