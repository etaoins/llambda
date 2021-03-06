(define-library (llambda char)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-stdlib-procedure define-stdlib)))

  (export char-alphabetic? char-numeric? char-whitespace? char-upper-case? char-lower-case? digit-value char-upcase
          char-downcase char-foldcase string-upcase string-downcase string-foldcase string-ci=? string-ci<? string-ci>?
          string-ci<=? string-ci>=? char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?)

  (begin
    (define-native-library llchar (static-library "ll_llambda_char"))

    (define-stdlib char-alphabetic? (native-function llchar "llchar_char_is_alphabetic" (-> <native-unicode-char> <native-bool>)))
    (define-stdlib char-numeric? (native-function llchar "llchar_char_is_numeric" (-> <native-unicode-char> <native-bool>)))
    (define-stdlib char-whitespace? (native-function llchar "llchar_char_is_whitespace" (-> <native-unicode-char> <native-bool>)))
    (define-stdlib char-upper-case? (native-function llchar "llchar_char_is_upper_case" (-> <native-unicode-char> <native-bool>)))
    (define-stdlib char-lower-case? (native-function llchar "llchar_char_is_lower_case" (-> <native-unicode-char> <native-bool>)))
    (define-stdlib char-upcase (native-function llchar "llchar_char_upcase" (-> <native-unicode-char> <native-unicode-char>)))
    (define-stdlib char-downcase (native-function llchar "llchar_char_downcase" (-> <native-unicode-char> <native-unicode-char>)))
    (define-stdlib char-foldcase (native-function llchar "llchar_char_foldcase" (-> <native-unicode-char> <native-unicode-char>)))
    (define-stdlib string-upcase (world-function llchar "llchar_string_upcase" (-> <string> <string>)))
    (define-stdlib string-downcase (world-function llchar "llchar_string_downcase" (-> <string> <string>)))
    (define-stdlib string-foldcase (world-function llchar "llchar_string_foldcase" (-> <string> <string>)))
    (define-stdlib digit-value (world-function llchar "llchar_digit_value" (-> <native-unicode-char> (U #f <integer>))))

    (define-stdlib string-ci=? (native-function llchar "llchar_string_ci_equal" (-> <string> <string> <string> * <native-bool>) nocapture))
    (define-stdlib string-ci<? (native-function llchar "llchar_string_ci_lt" (-> <string> <string> <string> * <native-bool>) nocapture))
    (define-stdlib string-ci>? (native-function llchar "llchar_string_ci_gt" (-> <string> <string> <string> * <native-bool>) nocapture))
    (define-stdlib string-ci<=? (native-function llchar "llchar_string_ci_lte" (-> <string> <string> <string> * <native-bool>) nocapture))
    (define-stdlib string-ci>=? (native-function llchar "llchar_string_ci_gte" (-> <string> <string> <string> * <native-bool>) nocapture))

    (define-stdlib char-ci=? (native-function llchar "llchar_char_ci_equal" (-> <native-unicode-char> <native-unicode-char> <char> * <native-bool>) nocapture))
    (define-stdlib char-ci<? (native-function llchar "llchar_char_ci_lt" (-> <native-unicode-char> <native-unicode-char> <char> * <native-bool>) nocapture))
    (define-stdlib char-ci>? (native-function llchar "llchar_char_ci_gt" (-> <native-unicode-char> <native-unicode-char> <char> * <native-bool>) nocapture))
    (define-stdlib char-ci<=? (native-function llchar "llchar_char_ci_lte" (-> <native-unicode-char> <native-unicode-char> <char> * <native-bool>) nocapture))
    (define-stdlib char-ci>=? (native-function llchar "llchar_char_ci_gte" (-> <native-unicode-char> <native-unicode-char> <char> * <native-bool>) nocapture))))
