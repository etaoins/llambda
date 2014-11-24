(define-library (scheme inexact)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))
  (import (only (scheme base) inexact eq?))

  ; inexact library
  (include-library-declarations "../../interfaces/scheme/inexact.scm")
  (begin
    (define-r7rs finite? (native-function "lliby_is_finite" (<number>) -> <native-bool>))
    (define-r7rs infinite? (native-function "lliby_is_infinite" (<number>) -> <native-bool>))
    
    (define-r7rs nan? (lambda ((n : <number>))
      (eq? n +nan.0)))

    ; These always return inexact numbers so we can use the C standard library. However, they need to be wrapped in a
    ; Scheme procedure to explicitly convert the number in to flonum.
    (define-r7rs sin (lambda ((num : <number>))
      (define native-sin (native-function "sin" (<native-double>) -> <native-double>))
      (native-sin (inexact num))))

    (define-r7rs cos (lambda ((num : <number>))
      (define native-cos (native-function "cos" (<native-double>) -> <native-double>))
      (native-cos (inexact num))))

    (define-r7rs tan (lambda ((num : <number>))
      (define native-tan (native-function "tan" (<native-double>) -> <native-double>))
      (native-tan (inexact num))))

    (define-r7rs asin (lambda ((num : <number>))
      (define native-asin (native-function "asin" (<native-double>) -> <native-double>))
      (native-asin (inexact num))))

    (define-r7rs acos (lambda ((num : <number>))
      (define native-acos (native-function "acos" (<native-double>) -> <native-double>))
      (native-acos (inexact num))))

    (define-r7rs atan (lambda ((num : <number>))
      (define native-atan (native-function "atan" (<native-double>) -> <native-double>))
      (native-atan (inexact num))))

    (define-r7rs sqrt (lambda ((num : <number>))
      (define native-sqrt (native-function "sqrt" (<native-double>) -> <native-double>))
      (native-sqrt (inexact num))))

    (define-r7rs exp (lambda ((num : <number>))
      (define native-exp (native-function "exp" (<native-double>) -> <native-double>))
      (native-exp (inexact num))))

    (define-r7rs log (lambda ((num : <number>))
      (define native-log (native-function "log" (<native-double>) -> <native-double>))
      (native-log (inexact num))))))
