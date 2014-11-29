(cond-expand (immutable-pairs
  (define-test "(reverse) is polymorphic" (expect-success
    (import (llambda typed))

    (define integer-list (typed-dynamic '(1 2 3) (Listof <exact-integer>)))
    (define reverse-list (reverse integer-list))

    (ann integer-list (Listof <exact-integer>))))))
