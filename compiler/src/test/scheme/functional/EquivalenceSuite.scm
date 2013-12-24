(define-test "#t and #t are eqv" (expect #t
	(eqv? #t #t)))

(define-test "#f and #f are eqv" (expect #t
	(eqv? #f #f)))

(define-test "constant 'test and 'test are eqv" (expect #t
	(eqv? 'test 'test)))

(define-test "calculated 'test and 'test are eqv" (expect #t
	(eqv? (string->symbol "test") (string->symbol "test"))))

(define-test "constant 'test and calculated 'test are eqv" (expect #t
	(eqv? 'test (string->symbol "test"))))

(define-test "constant -163 and -163 are eqv" (expect #t
	(eqv? -163 -163)))

(define-test "calculated -163 and -163 are eqv" (expect #t
	(eqv? (- 163) (- 163))))

(define-test "constant 153.5 and 153.5 are eqv" (expect #t
	(eqv? 153.5 153.5)))

(define-test "calculatued 153.5 and 153.5 are eqv" (expect #t
	(eqv? (- -153.5) (- -153.5))))

(define-test "+inf.0 and +inf.0 are eqv" (expect #t
	(eqv? +inf.0 +inf.0)))

(define-test "#\a and #\a are eqv" (expect #t
	(eqv? #\a #\a)))

(define-test "'() and '() are eqv" (expect #t
	(eqv? '() '())))

(define-test "pairs in the same location are eqv" (expect #t
	(define var1 '(a b))
	(define var2 var1)
	(eqv? var1 var2)))

(define-test "vectors in the same location are eqv" (expect #t
	(define var1 #(1 2 3))
	(define var2 var1)
	(eqv? var1 var2)))

(define-test "records in the same location are eqv" (expect #t
	(define-record-type <unit> (unit) unit?)
	(define var1 (unit))
	(define var2 var1)
	(eqv? var1 var2)))

(define-test "procedures in the same location are eqv" (expect #t
	(define (procecedure) 5)

	(define var1 procecedure)
	(define var2 var1)
	(eqv? var1 var2)))

(define-test "1 and #t are not eqv" (expect #f
	(eqv? 1 #t)))

(define-test "#t and #f are not eqv" (expect #f
	(eqv? #t #f)))

(define-test "'one and 'two are not eqv" (expect #f
	(eqv? 'one 'two)))

(define-test "-163 and -163.0 are not eqv" (expect #f
	(eqv? -163 -163.0)))

(define-test "-163 and 3435 are not eqv" (expect #f
	(eqv? -163 3435)))

(define-test "-163.5 and 3435.5 are not eqv" (expect #f
	(eqv? -163.5 3435.5)))

(define-test "#\a and #\b are not eqv" (expect #f
	(eqv? #\a #\b)))

(define-test "'() and '(1 2 3) are not eqv" (expect #f
	(eqv? '() '(1 2 3))))

(define-test "'two constructed lists are not eqv" (expect #f
	(eqv? (list 1 2 3) (list 1 2 3))))

(define-test "'two constructed vectors are not eqv" (expect #f
	(eqv? (vector 1 2 3) (vector 1 2 3))))

(define-test "'two different procedures are not eqv" (expect #f
	; If these returned different values it would be legal to merge them
	(define (procecedure1) 1)
	(define (procecedure2) 2)
	(eqv? procecedure1 procecedure2)))
