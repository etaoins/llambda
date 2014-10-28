(define-library (llambda typed)
	(import (llambda internal primitives))
  (import (llambda nfi))
  (import (only (scheme base) letrec call-with-values begin))

  ; Re-export from (llambda primitives) 
  (export define-type cast ann : define: define-record-type: lambda: case-lambda: make-predicate U Rec Listof Pairof
          List Vector Vectorof Values -> case-> )

  ; Export our type names
  (export <any> <list-element> <pair> <empty-list> <string> <symbol> <boolean> <number> <exact-integer> <flonum> <char>
          <vector> <bytevector> <procedure> <port> <unit> <eof-object>)

  ; Our own type constructors
  (export Assocof)

  ; These are new macros
  (export define-predicate let: let*: letrec*: letrec: let-values: let*-values:)

  (begin
    (define-type (Assocof KT VT) (Listof (Pairof KT VT)))
    (define-type <eof-object> <unit>))

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
                     (letrec*: ((name : type val) ...) body1 body2 ...))))

    (define-syntax let-values:
      (syntax-rules ()
                    ((let-values: (binding ...) body0 body1 ...)
                     (let-values: "bind"
                                 (binding ...) () (begin body0 body1 ...)))
                    ((let-values: "bind" () tmps body)
                     (let: tmps body))
                    ((let-values: "bind" ((b0 e0)
                                         binding ...) tmps body)
                     (let-values: "mktmp" b0 e0 ()
                                 (binding ...) tmps body))
                    ((let-values: "mktmp" () e0 args
                                 bindings tmps body)
                     (call-with-values
                       (lambda: () e0)
                       (lambda: args
                         (let-values: "bind"
                                     bindings tmps body))))
                    ((let-values: "mktmp" ([name : <type>] . b) e0 (arg ...)
                                 bindings (tmp ...) body)
                     (let-values: "mktmp" b e0 (arg ... [x : <type>])
                                 bindings (tmp ... (name : <type> x)) body))
                    ((let-values: "mktmp" a e0 (arg ...)
                                 bindings (tmp ...) body)
                     (call-with-values
                       (lambda: () e0)
                       (lambda: (arg ... . x)
                         (let-values: "bind"
                                     bindings (tmp ... (a x)) body))))))

    (define-syntax let*-values:
      (syntax-rules ()
                    ((let*-values: () body0 body1 ...)
                     (let: () body0 body1 ...))
                    ((let*-values: (binding0 binding1 ...)
                                   body0 body1 ...)
                     (let-values: (binding0)
                                  (let*-values: (binding1 ...)
                                                body0 body1 ...)))))))
