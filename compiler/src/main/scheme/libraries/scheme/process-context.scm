(define-library (scheme process-context)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))

  ; process-context library
  (include-library-declarations "../../interfaces/scheme/process-context.scm")
  (begin
    (define-r7rs exit (world-function "lliby_exit" (<any>) noreturn))
    (define-r7rs emergency-exit (native-function "lliby_emergency_exit" (<any>) noreturn))
    (define-r7rs get-environment-variable (world-function "lliby_get_environment_variable" (<string>) -> (U <string> #f)))
    (define-r7rs get-environment-variables (world-function "lliby_get_environment_variables" () -> (Listof (List <string> <string>))))
  ))
