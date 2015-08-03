(define-library (llambda typed)
	(import (llambda internal primitives))
  (import (llambda nfi))

  ; Re-export from (llambda primitives)
  (export define-type cast ann : make-predicate U Rec Listof Pairof List Values -> case-> All HashMap)

  ; Export our type names
  (export <any> <list-element> <pair> <empty-list> <string> <symbol> <boolean> <number> <exact-integer> <flonum> <char>
          <vector> <bytevector> <procedure> <port> <unit> <eof-object>)

  ; Type constructors
  (export Assocof)
  ; Macros
  (export define-predicate)

  (begin
    (define-type (Assocof KT VT) (Listof (Pairof KT VT)))

    (define-syntax define-predicate
      (syntax-rules ()
                    ((define-predicate name type)
                     (define name (make-predicate type)))))))
