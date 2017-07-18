(define-library (llambda r7rs-case-lambda)

(import (llambda base))
(export r7rs-case-lambda)

(begin
  (define-syntax r7rs-case-lambda
    (syntax-rules ()
                  ((r7rs-case-lambda (params body0 ...) ...)
                   (lambda args
                     (let ((len (length args)))
                       (let-syntax
                         ((cl (syntax-rules ::: ()
                                            ((cl)
                                             (error "no matching clause"))
                                            ((cl ((p :::) . body) . rest)
                                             (if (= len (length '(p :::)))
                                               (apply (lambda (p :::)
                                                        . body) args)
                                               (cl . rest)))
                                            ((cl ((p ::: . tail) . body)
                                                 . rest)
                                             (if (>= len (length '(p :::)))
                                               (apply
                                                 (lambda (p ::: . tail)
                                                   . body) args)
                                               (cl . rest))))))
                         (cl (params body0 ...) ...)))))))))
