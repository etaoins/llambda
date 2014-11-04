(define-test "(number->string)" (expect-success
  (assert-equal "0" (number->string 0))
  (assert-equal "9223372036854775807" (number->string 9223372036854775807))
  (assert-equal "-9223372036854775808" (number->string -9223372036854775808))

  (assert-equal "#b0" (number->string 0 2))
  (assert-equal "#b111111111111111111111111111111111111111111111111111111111111111" (number->string 9223372036854775807 2))
  (assert-equal "#b-1000000000000000000000000000000000000000000000000000000000000000" (number->string -9223372036854775808 2))

  (assert-equal "#o0" (number->string 0 8))
  (assert-equal "#o777777777777777777777" (number->string 9223372036854775807 8))
  (assert-equal "#o-1000000000000000000000" (number->string -9223372036854775808 8))

  (assert-equal "0" (number->string 0 10))
  (assert-equal "9223372036854775807" (number->string 9223372036854775807 10))
  (assert-equal "-9223372036854775808" (number->string -9223372036854775808 10))

  (assert-equal "#x0" (number->string 0 16))
  (assert-equal "#x7fffffffffffffff" (number->string 9223372036854775807 16))
  (assert-equal "#x-8000000000000000" (number->string -9223372036854775808 16))

  (assert-equal "+nan.0" (number->string +nan.0))
  (assert-equal "+nan.0" (number->string +nan.0 2))
  (assert-equal "+nan.0" (number->string +nan.0 8))
  (assert-equal "+nan.0" (number->string +nan.0 10))
  (assert-equal "+nan.0" (number->string +nan.0 16))

  (assert-equal "+inf.0" (number->string +inf.0))
  (assert-equal "+inf.0" (number->string +inf.0 2))
  (assert-equal "+inf.0" (number->string +inf.0 8))
  (assert-equal "+inf.0" (number->string +inf.0 10))
  (assert-equal "+inf.0" (number->string +inf.0 16))

  (assert-equal "-inf.0" (number->string -inf.0))
  (assert-equal "-inf.0" (number->string -inf.0 2))
  (assert-equal "-inf.0" (number->string -inf.0 8))
  (assert-equal "-inf.0" (number->string -inf.0 10))
  (assert-equal "-inf.0" (number->string -inf.0 16))

  (assert-equal "0.0" (number->string 0.0))
  (assert-equal "0.0" (number->string 0.0 10))

  (assert-equal "-0.0" (number->string -0.0))
  (assert-equal "-0.0" (number->string -0.0 10))

  (assert-equal "0.5" (number->string 0.5))
  (assert-equal "0.5" (number->string 0.5 10))

  (assert-equal "-0.5" (number->string -0.5))
  (assert-equal "-0.5" (number->string -0.5 10))

  (assert-equal "1.0" (number->string 1.0))
  (assert-equal "1.0" (number->string 1.0 10))

  (assert-equal "-1.0" (number->string -1.0))
  (assert-equal "-1.0" (number->string -1.0 10))))

(define-test "(number->string) in radix 3 is an error" (expect-failure
  (number->string 17 3)))

(define-test "(number->string) with non-decimal inexact is an error" (expect-failure
  (number->string 17.0 16)))

(define-test "(string->number)" (expect-success
  (assert-equal 100 (string->number "100"))
  (assert-equal 4 (string->number "100" 2))
  (assert-equal 64 (string->number "100" 8))
  (assert-equal 256 (string->number "100" 16))

  (assert-equal 100 (string->number "#d100" 8))

  (assert-equal +nan.0 (string->number "+nan.0" 8))
  (assert-equal -nan.0 (string->number "-nan.0" 8))
  (assert-equal +inf.0 (string->number "+inf.0" 8))
  (assert-equal -inf.0 (string->number "-inf.0" 8))

  (assert-equal -2.0 (string->number "-500/250"))

  (assert-equal #f (string->number "+" 16))
  (assert-equal #f (string->number "ddy" 16))
  (assert-equal #f (string->number "2 5" 16))
  ))

(define-test "(string->number) in radix 3 is an error" (expect-failure
  (string->number "17" 3)))
