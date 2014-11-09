(define-library (scheme file)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))

  (include-library-declarations "../../interfaces/scheme/file.scm")

  (begin
    (define-r7rs file-exists? (native-function "lliby_file_exists" (<string>) -> <native-bool>))))
