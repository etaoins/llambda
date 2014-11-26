(define-library (scheme r5rs)
  (import (scheme base))
  (import (scheme char))
  (import (scheme cxr))
  (import (scheme file))
  (import (scheme inexact))
  (import (scheme lazy))
  (import (scheme read))
  (import (scheme write))
  (import (llambda complex-stub))

  (include-library-declarations "../../interfaces/scheme/r5rs.scm")

  (begin
    (define exact->inexact inexact)
    (define inexact->exact exact)))
