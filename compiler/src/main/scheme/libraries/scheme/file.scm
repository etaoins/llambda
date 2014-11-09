(define-library (scheme file)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))

  (include-library-declarations "../../interfaces/scheme/file.scm")

  (begin
    (define-r7rs file-exists? (native-function "lliby_file_exists" (<string>) -> <native-bool>))

    (define-r7rs open-input-file (world-function "lliby_open_input_file" (<string>) -> <port>))
    (define-r7rs open-binary-input-file open-input-file)
    (define-r7rs open-output-file (world-function "lliby_open_output_file" (<string>) -> <port>))
    (define-r7rs open-binary-output-file open-output-file)))
