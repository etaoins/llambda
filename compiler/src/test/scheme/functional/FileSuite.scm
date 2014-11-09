(define-test "(file-exists?)" (expect-success
  (import (scheme file))

  (assert-true (file-exists? (path-for-test-file "empty-file")))
  (assert-false (file-exists? (path-for-test-file "does-not-exist")))))

(define-test "(open-input-file)" (expect-success
  (import (scheme file))

  (assert-raises file-error?
                 (open-input-file (path-for-test-file "does-not-exist")))

  (define empty-file (open-binary-input-file (path-for-test-file "empty-file")))

  (assert-true (port? empty-file))
  (assert-true (input-port? empty-file))
  (assert-true (input-port-open? empty-file))
  (assert-true (eof-object? (read-u8 empty-file)))

  (close-port empty-file)
  (assert-false (input-port-open? empty-file))

  (define utf8-file (open-input-file (path-for-test-file "utf8-file")))
  (assert-equal "溮煡煟" (read-string 3 utf8-file))
  (assert-equal #x20 (read-u8 utf8-file))
  (assert-equal #\鍹 (read-char utf8-file))
  (assert-equal "餳駷 厊圪妀 輠 轈鄻" (read-line utf8-file))
  (assert-equal #\☃ (read-char utf8-file))
  (assert-equal #\newline (read-char utf8-file))
  (assert-equal "" (read-line utf8-file))
  (assert-equal "Hello, world!" (read-line utf8-file))
  (assert-true (eof-object? (read-line utf8-file)))))
