(define-test "non-empty bytevector is a bytevector" (expect #t
	(bytevector? #u8(1 2 3))))

(define-test "empty bytevector is  bytevector" (expect #t
	(bytevector? #u8())))

(define-test "exact integer is not a bytevector" (expect #f
	(bytevector? 4)))

(define-test "(make-bytevector) an uninitialized empty bytevector" (expect #u8()
	(make-bytevector 0)))

(define-test "(make-bytevector) a filled empty bytevector" (expect #u8()
	(make-bytevector 0 0)))

(define-test "(make-bytevector) an uninitialized non-empty bytevector" (expect #u8(0 0 0)
	(make-bytevector 3)))

(define-test "(make-bytevector) a filled non-empty bytevector" (expect #u8(5 5 5)
	(make-bytevector 3 5)))

(define-test "(make-bytevector) a inexact rational fails" (expect-failure
	(make-bytevector 3 5.0)))

(define-test "(bytevector) an empty bytevector" (expect #u8()
	(bytevector)))

(define-test "(bytevector) a non-empty bytevector" (expect #u8(1 3 5 1 3 5)
	(bytevector 1 3 5 1 3 5)))

(define-test "bytevector length of non-empty constant bytevector" (expect 3
	(bytevector-length #u8(1 2 3))))

(define-test "vector length of empty constant bytevector" (expect 0
	(bytevector-length #u8())))

(define-test "bytevector length of non-empty constructed bytevector" (expect 15
	(bytevector-length (make-bytevector 15 129))))

(define-test "bytevector length of empty constructed bytevector" (expect 0
	(bytevector-length (make-bytevector 0 15))))

(define-test "bytevector-u8-ref" (expect 5
	(bytevector-u8-ref #u8(1 3 5 201 203 205) 2)))

(define-test "bytevector-u8-ref out of bounds fails" (expect-failure
	(bytevector-u8-ref #u8(1 3 5 201 203 205) 7)))

(define-test "bytevector-u8-ref with non-integer fails" (expect-failure
	(bytevector-u8-ref #u8(1 3 5 201 203 205) "4")))

(define-test "bytevector-u8-set!" (expect #u8(1 1 2 1 1)
	; Need to make a new bytevector because vector literals are immutable
	(define test-bytevector (make-bytevector 5 1))
	(bytevector-u8-set! test-bytevector 2 2)
	test-bytevector))

(define-test "bytevector-u8-set! on bytevector literal fails" (expect-failure
	; We should fail gracefully from this - i.e. no segfault, no silent success
	(bytevector-u8-set! #u8(1 1 1 1 1 1) 2 2)))

(define-test "(bytevector-append) with no arguments" (expect #u8()
	(bytevector-append)))

(define-test "(bytevector-append) with single argument" (expect #u8(1 2 3)
	(bytevector-append #u8(1 2 3))))

(define-test "(bytevector-append) three bytevectors" (expect #u8(1 2 3 4 5 6)
	(bytevector-append #u8(1 2) #u8(3 4) #u8(5 6))))

(define-test "(bytevector-append) three empty bytevectors" (expect #u8()
	(bytevector-append #u8() #u8() #u8())))

(define-test "(bytevector-append) with non-bytevector fails" (expect-failure
	(bytevector-append #u8(1 2) #(3 4))))

(define-test "(utf8->string)" (expect-success
  (assert-equal "" (utf8->string #u8()))
  (assert-equal "A" (utf8->string #u8(#x41)))
  (assert-equal "Hell☃!" (utf8->string #u8(#x48 #x65 #x6c #x6c #xe2 #x98 #x83 #x21)))
  (assert-equal "☃!" (utf8->string #u8(#x48 #x65 #x6c #x6c #xe2 #x98 #x83 #x21) 4))
  (assert-equal "" (utf8->string #u8(#x48 #x65 #x6c #x6c #xe2 #x98 #x83 #x21) 0 0))
  (assert-equal "" (utf8->string #u8(#x48 #x65 #x6c #x6c #xe2 #x98 #x83 #x21) 8 8))
  (assert-equal "ell☃" (utf8->string #u8(#x48 #x65 #x6c #x6c #xe2 #x98 #x83 #x21) 1 7))))

(define-test "(utf8->string) with backwards slice fails" (expect-failure
  (utf8->string #u8(#x48 #x65 #x6c #x6c #xe2 #x98 #x83 #x21) 2 1)))

(define-test "(utf8->string) past end of bytevector fails" (expect-failure
  (utf8->string #u8(#x48 #x65 #x6c #x6c #xe2 #x98 #x83 #x21) 0 9)))

(define-test "(utf8->string) with negative start index fails" (expect-failure
  (utf8->string #u8(#x48 #x65 #x6c #x6c #xe2 #x98 #x83 #x21) -1)))

(define-test "(string->utf8)" (expect-success
  (assert-equal #u8() (string->utf8 ""))
  (assert-equal #u8(#xce #xbb) (string->utf8 "λ"))
  (assert-equal #u8(#x48 #x65 #x6c #x6c #xe2 #x98 #x83 #x21) (string->utf8 "Hell☃!"))
  (assert-equal #u8(#xe2 #x98 #x83 #x21) (string->utf8 "Hell☃!" 4))
  (assert-equal #u8() (string->utf8 "Hell☃!" 0 0))
  (assert-equal #u8() (string->utf8 "Hell☃!" 5 5))
  (assert-equal #u8(#xe2 #x98 #x83) (string->utf8 "Hell☃!" 4 5))))

(define-test "(string->utf8) with backwards slice fails" (expect-failure
  (string->utf8 "Hell☃!" 2 1)))

(define-test "(string->utf8) past end of string fails" (expect-failure
  (string->utf8 "Hell☃!" 0 9)))

(define-test "(string->utf8) with negative start index fails" (expect-failure
  (string->utf8 "Hell☃!" -1)))
