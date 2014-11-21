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

(define-test "(symbol=?) with one arg fails" (expect-compile-failure
	(symbol=? 'test)))

(define-test "(symbol=?) with non-symbol fails" (expect-compile-failure
	(symbol=? 'test 'test "test")))

(define-test "(symbol->string)" (expect-success
  ; Inline symbol
	(assert-equal "flying-cat" (symbol->string 'flying-cat))
  ; Heap symbol
	(assert-equal "flying-hippopotamus" (symbol->string 'flying-hippopotamus))))

(define-test "(string->symbol)" (expect-success
	(assert-equal 'mISSISSIppi (string->symbol "mISSISSIppi"))
	(assert-equal 'MassaCHUsetts (string->symbol "MassaCHUsetts"))))

(define-test "(string->symbol (symbol->string))" (expect-success
	(assert-equal 'LollyPop (string->symbol (symbol->string 'LollyPop)))
	(assert-equal 'SourPatchKids (string->symbol (symbol->string 'SourPatchKids)))))
