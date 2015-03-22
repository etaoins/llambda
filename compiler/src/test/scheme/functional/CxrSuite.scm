; These procedures are programmatically generated so testing one should be almost as good as testing them all
(define-test "(caddr)" (expect-static-success
  (import (scheme cxr))
  (assert-equal 3 (caddr '(1 2 3 4)))))
