(define-library (scheme inexact)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))
  (import (only (scheme base) inexact eq?))

  ; inexact library
  (include-library-declarations "../../interfaces/scheme/inexact.scm")
  (begin
    (define-r7rs finite? (native-function system-library "lliby_is_finite" (<number>) -> <native-bool>))
    (define-r7rs infinite? (native-function system-library "lliby_is_infinite" (<number>) -> <native-bool>))

    (define-r7rs (nan? [n : <number>])
      (eq? n +nan.0))

    ; These always return inexact numbers so we can use the C standard library. However, they need to be wrapped in a
    ; Scheme procedure to explicitly convert the number in to flonum.
    (define-r7rs (sin [num : <number>])
      (define native-sin (native-function system-library "sin" (<native-double>) -> <native-double>))
      (native-sin (inexact num)))

    (define-r7rs (cos [num : <number>])
      (define native-cos (native-function system-library "cos" (<native-double>) -> <native-double>))
      (native-cos (inexact num)))

    (define-r7rs (tan [num : <number>])
      (define native-tan (native-function system-library "tan" (<native-double>) -> <native-double>))
      (native-tan (inexact num)))

    (define-r7rs (asin [num : <number>])
      (define native-asin (native-function system-library "asin" (<native-double>) -> <native-double>))
      (native-asin (inexact num)))

    (define-r7rs (acos [num : <number>])
      (define native-acos (native-function system-library "acos" (<native-double>) -> <native-double>))
      (native-acos (inexact num)))

    (define-r7rs (atan [num : <number>])
      (define native-atan (native-function system-library "atan" (<native-double>) -> <native-double>))
      (native-atan (inexact num)))

    (define-r7rs (sqrt [num : <number>])
      (define native-sqrt (native-function system-library "sqrt" (<native-double>) -> <native-double>))
      (native-sqrt (inexact num)))

    (define-r7rs (exp [num : <number>])
      (define native-exp (native-function system-library "exp" (<native-double>) -> <native-double>))
      (native-exp (inexact num)))

    (define-r7rs (log [num : <number>])
      (define native-log (native-function system-library "log" (<native-double>) -> <native-double>))
      (native-log (inexact num)))))
