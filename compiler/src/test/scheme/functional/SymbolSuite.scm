(define-test "symbol constant is a symbol" (expect #t
	(symbol? 'test)))

(define-test "empty list is not a symbol" (expect #f
	(string? '())))

(define-test "symbol=? with one arg fails" (expect-failure
	(symbol=? 'test)))

(define-test "symbol=? with two equal args" (expect #t
	(symbol=? 'test 'test)))

(define-test "symbol=? with two inequal args" (expect #f
	(symbol=? 'test 'nottest)))

(define-test "symbol=? with three equal args" (expect #t
	(symbol=? 'test 'test 'test)))

(define-test "symbol=? with three inequal args" (expect #f
	(symbol=? 'test 'test 'nottest)))

(define-test "symbol=? with non-symbol fails" (expect-failure
	(symbol=? 'test 'test "test")))

(define-test "symbol->string" (expect "flying-fish"
	(symbol->string 'flying-fish)))

(define-test "string->symbol" (expect mISSISSIppi
	(string->symbol "mISSISSIppi")))

(define-test "(string->symbol (symbol->string))" (expect LollyPop
	(string->symbol (symbol->string 'LollyPop))))
