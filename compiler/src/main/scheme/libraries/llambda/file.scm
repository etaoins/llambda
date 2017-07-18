(define-library (llambda file)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-stdlib-procedure define-stdlib)))
  (import (only (llambda base) call-with-port current-input-port current-output-port))

  (export file-exists? open-input-file open-binary-input-file open-output-file open-binary-output-file
          call-with-input-file call-with-output-file with-input-from-file with-output-to-file delete-file)

  (begin
    (define-native-library llfile (static-library "ll_llambda_file"))

    (define-stdlib file-exists? (native-function llfile "llfile_file_exists" (-> <string> <native-bool>) nocapture))

    (define-stdlib open-input-file (world-function llfile "llfile_open_input_file" (-> <string> <port>)))
    (define-stdlib open-binary-input-file open-input-file)

    (define-stdlib (call-with-input-file [path : <string>] [proc : (-> <port> <any>)])
                 (call-with-port (open-input-file path) proc))

    (define-stdlib (with-input-from-file [path : <string>] [proc : (-> <any>)])
                 (call-with-input-file path (lambda ([port : <port>])
                                              (parameterize ((current-input-port port))
                                                            (proc)))))

    (define-stdlib open-output-file (world-function llfile "llfile_open_output_file" (-> <string> <port>)))
    (define-stdlib open-binary-output-file open-output-file)

    (define-stdlib (call-with-output-file [path : <string>] [proc : (-> <port> <any>)])
                 (call-with-port (open-output-file path) proc))

    (define-stdlib (with-output-to-file [path : <string>] [proc : (-> <any>)])
                 (call-with-output-file path (lambda ([port : <port>])
                                               (parameterize ((current-output-port port))
                                                             (proc)))))

    (define-stdlib delete-file (world-function llfile "llfile_delete_file" (-> <string> <unit>)))))
