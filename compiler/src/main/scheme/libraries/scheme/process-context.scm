(define-library (scheme process-context)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-stdlib-procedure define-stdlib)))

  ; process-context library
  (include-library-declarations "../../interfaces/scheme/process-context.scm")
  (begin
    (define-native-library llprocesscontext (static-library "ll_scheme_processcontext"))

    (define-stdlib exit (world-function llprocesscontext "llprocesscontext_exit" (-> <any> <unit>) noreturn))
    (define-stdlib get-environment-variable (world-function llprocesscontext "llprocesscontext_get_environment_variable" (-> <string> (U <string> #f))))
    (define-stdlib get-environment-variables (world-function llprocesscontext "llprocesscontext_get_environment_variables" (-> (Listof (List <string> <string>)))))
    (define-stdlib command-line (world-function llprocesscontext "llprocesscontext_command_line" (-> (Listof <string>))))))
