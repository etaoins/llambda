; export (lambda primitives) that occur in (scheme base)
; these are virtual definitions provided by the compiler
(export lambda quote if set! syntax-error include quasiquote unquote
        unquote-splicing define define-syntax define-record-type
        cond-expand parameterize)

(export begin)
(export let let* letrec* letrec let-syntax letrec-syntax)
(export cond case and or when unless)
(export do)
(export eqv? eq? equal?)
(export number? complex? real? rational? exact? integer? exact-integer? inexact? finite? infinite? nan? zero? even? odd?
        exact inexact + - / * = < > <= >= positive? negative?)
(export boolean? not boolean=?)
(export pair? null? list? cons car cdr set-car! set-cdr! length list-copy make-list list append memv memq member)
(export symbol? symbol=? symbol->string string->symbol)
(export char? digit-value char->integer integer->char)
(export vector? make-vector vector vector-length vector-ref vector-set! list->vector vector-append)
(export bytevector? make-bytevector bytevector bytevector-length bytevector-u8-ref bytevector-u8-set! bytevector-append)
(export string? make-string string string-length string-ref string-set! string-append list->string)
(export procedure? apply call-with-current-continuation call/cc)
(export make-parameter dynamic-wind)
(export port? input-port? output-port? current-input-port current-output-port current-error-port)
(export newline)
(export features)
(export with-exception-handler raise error error-object? error-object-message error-object-irritants)
