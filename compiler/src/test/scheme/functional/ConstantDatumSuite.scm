; Inline strings are 11 bytes or less
; They have their UTF-8 data stored inline in their cell
(define-test "inline string" (expect "Hi there!"
  "Hi there!"))

; Heap strings are 12 bytes or more
; They have their UTF-8 data stored externally to their cell
(define-test "heap string" (expect "Greetings dear neighbour!!"
  "Greetings dear neighbour!!"))

(define-test "integer" (expect 31337
  31337))

(define-test "integer greater than 32bits" (expect 9007199254740993
  9007199254740993))

(define-test "very large flonum" (expect 9007199254740992.0
  9007199254740992.0))

(define-test "flonum" (expect -12.0
  -12.0))

(define-test "positive infinity" (expect +inf.0
  +inf.0))

(define-test "negative infinity" (expect +inf.0
  +inf.0))

(define-test "boolean true" (expect #t
  #t))

(define-test "boolean false" (expect #f
  #f))

(define-test "inline symbol" (expect symbol
  'symbol))

(define-test "heap symbol" (expect definitely-heap-symbol
  'definitely-heap-symbol))

(define-test "unit" (expect #!unit
  '#!unit))

(define-test "empty list" (expect ()
  '()))

(define-test "character" (expect #\a
  #\a))

(define-test "proper list" (expect (1 2 3 4)
  '(1 2 3 4)))

(define-test "improper list" (expect (1 2 3 . 4)
  '(1 2 3 . 4)))

(define-test "vector" (expect #(a b c)
  #(a b c)))

(define-test "bytevector" (expect #u8(0 1 127 128 255)
  #u8(0 1 127 128 255)))

(define-test "complex structure" (expect (0 . (#t #(a #\b #u8(3)) 4 #("five" "six") 7.0 #t))
  '(0 . (#t #(a #\b #u8(3)) 4 #("five" "six") 7.0 #t)) ))
