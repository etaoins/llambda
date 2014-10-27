(define-library (scheme char)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))

  ; char library
  (include-library-declarations "../../interfaces/scheme/char.scm")

  (begin
    (define-r7rs char-alphabetic? (native-function "lliby_char_is_alphabetic" (<native-unicode-char>) -> <native-bool>))
    (define-r7rs char-numeric? (native-function "lliby_char_is_numeric" (<native-unicode-char>) -> <native-bool>))
    (define-r7rs char-whitespace? (native-function "lliby_char_is_whitespace" (<native-unicode-char>) -> <native-bool>))
    (define-r7rs char-upper-case? (native-function "lliby_char_is_upper_case" (<native-unicode-char>) -> <native-bool>))
    (define-r7rs char-lower-case? (native-function "lliby_char_is_lower_case" (<native-unicode-char>) -> <native-bool>))
    (define-r7rs char-upcase (native-function "lliby_char_upcase" (<native-unicode-char>) -> <native-unicode-char>))
    (define-r7rs char-downcase (native-function "lliby_char_downcase" (<native-unicode-char>) -> <native-unicode-char>))
    (define-r7rs char-foldcase (native-function "lliby_char_foldcase" (<native-unicode-char>) -> <native-unicode-char>))
    (define-r7rs string-upcase (world-function "lliby_string_upcase" (<string>) -> <string>))
    (define-r7rs string-downcase (world-function "lliby_string_downcase" (<string>) -> <string>))
    (define-r7rs string-foldcase (world-function "lliby_string_foldcase" (<string>) -> <string>))
    (define-r7rs digit-value (world-function "lliby_digit_value" (<native-unicode-char>) -> <any>))

    (define-r7rs string-ci=? (native-function "lliby_string_ci_equal" (<string> <string> . <string>) -> <native-bool>))
    (define-r7rs string-ci<? (native-function "lliby_string_ci_lt" (<string> <string> . <string>) -> <native-bool>))
    (define-r7rs string-ci>? (native-function "lliby_string_ci_gt" (<string> <string> . <string>) -> <native-bool>))
    (define-r7rs string-ci<=? (native-function "lliby_string_ci_lte" (<string> <string> . <string>) -> <native-bool>))
    (define-r7rs string-ci>=? (native-function "lliby_string_ci_gte" (<string> <string> . <string>) -> <native-bool>))))
