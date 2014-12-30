; This file is automatically generated by generate-error-categories.scm. Do not edit manually.
(define-library (llambda error)
  (import (scheme base))
  (import (llambda typed))
  (import (llambda nfi))

  (export type-error? arity-error? range-error? utf8-error? divide-by-zero-error? mutate-literal-error? undefined-variable-error? out-of-memory-error? invalid-argument-error?)
  (begin
    (define-native-library llerror (static-library "ll_llambda_error"))
    (define type-error? (native-function llerror "llerror_is_type_error" (-> <any> <native-bool>)))
    (define arity-error? (native-function llerror "llerror_is_arity_error" (-> <any> <native-bool>)))
    (define range-error? (native-function llerror "llerror_is_range_error" (-> <any> <native-bool>)))
    (define utf8-error? (native-function llerror "llerror_is_utf8_error" (-> <any> <native-bool>)))
    (define divide-by-zero-error? (native-function llerror "llerror_is_divide_by_zero_error" (-> <any> <native-bool>)))
    (define mutate-literal-error? (native-function llerror "llerror_is_mutate_literal_error" (-> <any> <native-bool>)))
    (define undefined-variable-error? (native-function llerror "llerror_is_undefined_variable_error" (-> <any> <native-bool>)))
    (define out-of-memory-error? (native-function llerror "llerror_is_out_of_memory_error" (-> <any> <native-bool>)))
    (define invalid-argument-error? (native-function llerror "llerror_is_invalid_argument_error" (-> <any> <native-bool>)))
))
