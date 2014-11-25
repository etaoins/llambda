(define-library (scheme process-context)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))

  ; process-context library
  (include-library-declarations "../../interfaces/scheme/process-context.scm")
  (begin
    (define-r7rs exit (world-function system-library "lliby_exit" (<any>) noreturn))
    (define-r7rs emergency-exit (native-function system-library "lliby_emergency_exit" (<any>) noreturn))
    (define-r7rs get-environment-variable (world-function system-library "lliby_get_environment_variable" (<string>) -> (U <string> #f)))
    (define-r7rs get-environment-variables (world-function system-library "lliby_get_environment_variables" () -> (Listof (List <string> <string>))))
    (define-r7rs command-line (world-function system-library "lliby_command_line" () -> (Listof <string>)))))
