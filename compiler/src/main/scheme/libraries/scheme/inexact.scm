(define-library (scheme inexact)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))
  (import (only (scheme base) inexact))

  ; inexact library
  (include-library-declarations "../../interfaces/scheme/inexact.scm")
  (begin
    ; These always return inexact numbers so we can use the C standard library. However, they need to be wrapped in a
    ; Scheme procedure to explicitly convert the number in to flonum.
    (define-r7rs sin (lambda: ((num : <number>))
      (define native-sin (native-function "sin" (<native-double>) -> <native-double>))
      (native-sin (inexact num))))

    (define-r7rs cos (lambda: ((num : <number>))
      (define native-cos (native-function "cos" (<native-double>) -> <native-double>))
      (native-cos (inexact num))))

    (define-r7rs tan (lambda: ((num : <number>))
      (define native-tan (native-function "tan" (<native-double>) -> <native-double>))
      (native-tan (inexact num))))))
