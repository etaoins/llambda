(define-library (llambda typed)
	(import (llambda internal primitives))
  (import (llambda nfi))
  (import (only (scheme base) letrec))

  ; Re-export from (llambda primitives) 
  (export define-type cast ann : define: define-record-type: lambda: case-lambda: make-predicate U Rec Listof Pairof
          List Vector Vectorof Values -> case-> )

  ; Export our type names
  (export <any> <list-element> <pair> <empty-list> <string> <symbol> <boolean> <number> <exact-integer> <flonum> <char>
          <vector> <bytevector> <procedure> <port> <unit>)

  ; Our own type constructors
  (export Assocof)

  ; These are new macros
  (export define-predicate let: let*: letrec*: letrec:)
  
  (begin
    (define-type (Assocof KT VT) (Listof (Pairof KT VT))))

  (begin 
    (define-syntax define-predicate
      (syntax-rules ()
                    ((define-predicate name type)
                     (define name (make-predicate type)))))

    (define-syntax let:
      (syntax-rules (:)
                    ((let: ((name : type val) ...) body1 body2 ...)
                     ((lambda: ((name : type) ...) body1 body2 ...)
                      val ...))
                    ((let: tag ((name : type val) ...) body1 body2 ...)
                     ((letrec ((tag (lambda: ([name : type] ...)
                                      body1 body2 ...)))
                        tag)
                      val ...))))

    (define-syntax let*:
      (syntax-rules (:)
                    ((let*: () body1 body2 ...)
                     (let: () body1 body2 ...))
                    ((let*: ((name1 : type1 val1) (name2 : type2 val2) ...)
                       body1 body2 ...)
                     (let: ((name1 : type1 val1))
                       (let*: ((name2 : type2 val2) ...)
                         body1 body2 ...)))))

    (define-syntax letrec*:
      (syntax-rules (:)
                    ((letrec*: ((name : type val) ...) body1 body2 ...)
                     ((lambda ()
                        (define: name : type val) ...
                        body1 body2 ...)))))

    (define-syntax letrec:
      (syntax-rules (:)
                    ((letrec: ((name : type val) ...) body1 body2 ...)
                     (letrec*: ((name : type val) ...) body1 body2 ...)))))
)
