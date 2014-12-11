(define-library (scheme process-context)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))

  ; process-context library
  (include-library-declarations "../../interfaces/scheme/process-context.scm")
  (begin
    (define-native-library llprocesscontext (static-library "ll_scheme_processcontext"))

    (define-r7rs exit (world-function llprocesscontext "llprocesscontext_exit" (-> <any> <unit>) noreturn))
    (define-r7rs emergency-exit (native-function llprocesscontext "llprocesscontext_emergency_exit" (-> <any> <unit>) noreturn))
    (define-r7rs get-environment-variable (world-function llprocesscontext "llprocesscontext_get_environment_variable" (-> <string> (U <string> #f))))
    (define-r7rs get-environment-variables (world-function llprocesscontext "llprocesscontext_get_environment_variables" (-> (Listof (List <string> <string>)))))
    (define-r7rs command-line (world-function llprocesscontext "llprocesscontext_command_line" (-> (Listof <string>))))))
