(define-library (scheme char)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))

  ; char library
  (include-library-declarations "../../interfaces/scheme/char.scm")

  (begin
    (define-r7rs digit-value (world-function "lliby_digit_value" (<native-unicode-char>) -> <any>))))
