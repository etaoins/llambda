(define-test "(file-exists?)" (expect-success
  (import (scheme file))

  (assert-true (file-exists? (path-for-test-file "empty-file")))
  (assert-false (file-exists? (path-for-test-file "does-not-exist")))))
