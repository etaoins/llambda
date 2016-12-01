(define-library (llambda flonum)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-stdlib-procedure define-stdlib)))
  (import (only (scheme base) flonum eqv?))

  (export infinite? finite? nan? cos sin tan asin acos atan sqrt exp log)

  ; flonum library
  (begin
    (define-native-library llflonum (static-library "ll_llambda_flonum"))

    (define-stdlib finite? (native-function llflonum "llflonum_is_finite" (-> <number> <native-bool>)))
    (define-stdlib infinite? (native-function llflonum "llflonum_is_infinite" (-> <number> <native-bool>)))

    (define-stdlib (nan? [n : <number>])
      (eqv? n +nan.0))

    ; These always return flonum numbers so we can use the C standard library. However, they need to be wrapped in a
    ; Scheme procedure to explicitly convert the number in to flonum.
    (define-stdlib (sin [num : <number>])
      (define native-sin (native-function system-library "sin" (-> <native-double> <native-double>)))
      (native-sin (flonum num)))

    (define-stdlib (cos [num : <number>])
      (define native-cos (native-function system-library "cos" (-> <native-double> <native-double>)))
      (native-cos (flonum num)))

    (define-stdlib (tan [num : <number>])
      (define native-tan (native-function system-library "tan" (-> <native-double> <native-double>)))
      (native-tan (flonum num)))

    (define-stdlib (asin [num : <number>])
      (define native-asin (native-function system-library "asin" (-> <native-double> <native-double>)))
      (native-asin (flonum num)))

    (define-stdlib (acos [num : <number>])
      (define native-acos (native-function system-library "acos" (-> <native-double> <native-double>)))
      (native-acos (flonum num)))

    (define-stdlib (atan [num : <number>])
      (define native-atan (native-function system-library "atan" (-> <native-double> <native-double>)))
      (native-atan (flonum num)))

    (define-stdlib (sqrt [num : <number>])
      (define native-sqrt (native-function system-library "sqrt" (-> <native-double> <native-double>)))
      (native-sqrt (flonum num)))

    (define-stdlib (exp [num : <number>])
      (define native-exp (native-function system-library "exp" (-> <native-double> <native-double>)))
      (native-exp (flonum num)))

    (define-stdlib (log [num : <number>])
      (define native-log (native-function system-library "log" (-> <native-double> <native-double>)))
      (native-log (flonum num)))))
