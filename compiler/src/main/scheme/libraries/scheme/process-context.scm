(define-library (scheme process-context)
  (import (llambda nfi))
  (import (only (scheme base) cond eqv? else))
  (import (rename (llambda internal primitives) (define-stdlib-procedure define-stdlib)))

  (export exit get-environment-variable get-environment-variables command-line)

  (begin
    (define-native-library llprocesscontext (static-library "ll_scheme_processcontext"))

    (define native-exit (native-function system-library "exit" (-> <native-int32> <unit>) noreturn))

    (define-stdlib (exit (val : (U <integer> <boolean>)))
      (define exit-code : <integer>
        (cond
          ((eqv? val #t) 0)
          ((eqv? val #f) -1)
          (else val)))

      (native-exit exit-code))

    (define-stdlib get-environment-variable (world-function llprocesscontext "llprocesscontext_get_environment_variable" (-> <string> (U <string> #f))))
    (define-stdlib get-environment-variables (world-function llprocesscontext "llprocesscontext_get_environment_variables" (-> (Listof (List <string> <string>)))))
    (define-stdlib command-line (world-function llprocesscontext "llprocesscontext_command_line" (-> (Listof <string>))))))
