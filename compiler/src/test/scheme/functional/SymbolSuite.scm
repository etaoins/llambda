; This test assumes the inline -> heap transition happens after 12 bytes
(define-test "symbol constant is a symbol" (expect #t
	(symbol? 'test)))

(define-test "symbol constant of maximum inline size" (expect crash-length
	'crash-length))

(define-test "empty list is not a symbol" (expect #f
	(string? '())))

(define-test "symbol=? with one arg fails" (expect-failure
	(symbol=? 'test)))

(define-test "symbol=? with two equal args" (expect #t
	(symbol=? 'test 'test)))

(define-test "symbol=? with two inequal args" (expect #f
	(symbol=? 'test 'nottest)))

(define-test "symbol=? with three equal inline args" (expect #t
	(symbol=? 'test 'test 'test)))

(define-test "symbol=? with three equal heap args" (expect #t
	(symbol=? 'very-long-test-symbol 'very-long-test-symbol 'very-long-test-symbol)))

(define-test "symbol=? with three inequal args" (expect #f
	(symbol=? 'test 'test 'nottest)))

(define-test "symbol=? with non-symbol fails" (expect-failure
	(symbol=? 'test 'test "test")))

(define-test "symbol->string with inline symbol" (expect "flying-cat"
	(symbol->string 'flying-cat)))

(define-test "symbol->string with heap symbol" (expect "flying-hippopotamus"
	(symbol->string 'flying-hippopotamus)))

(define-test "string->symbol with inline symbol" (expect mISSISSIppi
	(string->symbol "mISSISSIppi")))

(define-test "string->symbol with heap symbol" (expect MassaCHUsetts
	(string->symbol "MassaCHUsetts")))

(define-test "(string->symbol (symbol->string)) with inline symbol" (expect LollyPop
	(string->symbol (symbol->string 'LollyPop))))

(define-test "(string->symbol (symbol->string)) with heap symbol" (expect SourPatchKids
	(string->symbol (symbol->string 'SourPatchKids))))
