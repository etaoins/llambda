(define-test "(file-exists?)" (expect-success
  (import (scheme file))

  (assert-true (file-exists? (path-for-test-file "empty-file")))
  (assert-false (file-exists? (path-for-test-file "path/does-not-exist")))))

(define-test "(open-input-file)" (expect-success
  (import (scheme file))

  (define missing-path (path-for-test-file "path/does-not-exist"))

  (assert-raises file-error?
                 (open-input-file missing-path))

  (assert-raises file-error?
                 (call-with-input-file missing-path (lambda (unused-file))))

  (assert-raises file-error?
                 (with-input-from-file missing-path (lambda ())))

  (define empty-file (open-binary-input-file (path-for-test-file "empty-file")))

  (assert-true (port? empty-file))
  (assert-true (input-port? empty-file))
  (assert-true (input-port-open? empty-file))
  (assert-true (eof-object? (read-u8 empty-file)))

  (close-port empty-file)
  (assert-false (input-port-open? empty-file))

  (call-with-input-file (path-for-test-file "utf8-file") (lambda (utf8-file)
    (assert-equal "溮煡煟" (read-string 3 utf8-file))
    (assert-equal #x20 (read-u8 utf8-file))
    (assert-equal #\鍹 (read-char utf8-file))
    (assert-equal "餳駷 厊圪妀 輠 轈鄻" (read-line utf8-file))
    (assert-equal #\☃ (read-char utf8-file))
    (assert-equal #\newline (read-char utf8-file))
    (assert-equal "" (read-line utf8-file))
    (assert-equal "Hello, world!" (read-line utf8-file))
    (assert-true (eof-object? (read-line utf8-file)))))

  (with-input-from-file (path-for-test-file "utf8-file") (lambda ()
    (assert-equal "溮煡煟 鍹餳駷 厊圪妀 輠 轈鄻" (read-line))))
  ))

(define-test "(open-output-file)" (expect-success
  (import (scheme file))

  (define missing-path (path-for-test-file "path/does-not-exist"))

  (assert-raises file-error?
                 (open-output-file missing-path))

  (assert-raises file-error?
                 (call-with-output-file missing-path (lambda (unused-file))))

  (assert-raises file-error?
                 (with-output-to-file missing-path (lambda ())))

  (define null-file (open-output-file "/dev/null"))

  (assert-true (port? null-file))
  (assert-true (output-port? null-file))
  (assert-true (output-port-open? null-file))
  (write-string "Hello, world!" null-file)

  (close-port null-file)
  (assert-false (output-port-open? null-file))

  (define captured-port #f)

  (call-with-output-file "/dev/null" (lambda (second-null)
    (set! captured-port second-null)
    (assert-true (output-port-open? second-null))))

  (assert-false (output-port-open? captured-port))

  (with-output-to-file "/dev/null" (lambda ()
    (write-string "HELLO!")))))

(define-test "(delete-file)" (expect-success
  (import (scheme file))

  (assert-raises file-error?
                 (delete-file (path-for-test-file "path/does-not-exist")))))
