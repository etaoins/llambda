; export (lambda primitives) that occur in (scheme base)
; these are virtual definitions provided by the compiler
(export lambda quote if set! syntax-error include quasiquote unquote
        unquote-splicing define define-syntax define-record-type
        cond-expand parameterize)

(export begin)
(export let let* letrec* letrec let-syntax letrec-syntax let-values let*-values)
(export cond case and or when unless)
(export do)
(export _ ...)
(export =>)
(export eqv? eq? equal?)
(export number? complex? real? rational? exact? integer? exact-integer? inexact? zero? even? odd? exact inexact + - / *
        expt = < > <= >= positive? negative? floor ceiling truncate round square abs truncate/ truncate-quotient
        truncate-remainder floor/ floor-quotient floor-remainder quotient remainder modulo max min gcd lcm
        exact-integer-sqrt)
(export number->string string->number)
(export boolean? not boolean=?)
(export pair? null? list? cons car cdr caar cadr cdar cddr length list-copy make-list list append memv memq member assv
        assq assoc reverse list-tail list-ref)

(cond-expand ((not immutable-pairs)
  (export set-car! set-cdr! list-set!)))

(export symbol? symbol=? symbol->string string->symbol)
(export char? char->integer integer->char char=? char<? char>? char<=? char>=?)
(export vector? make-vector vector vector-length vector-ref vector-set! list->vector vector->list vector-append
        vector-copy vector-copy! vector-fill! string->vector vector->string)
(export bytevector? make-bytevector bytevector bytevector-length bytevector-u8-ref bytevector-u8-set! bytevector-append
        utf8->string string->utf8 bytevector-copy bytevector-copy!)
(export string? make-string string string-length string-ref string-set! string-append list->string string->list
        string-copy string-copy! substring string-fill! string=? string<? string>? string<=? string>=?)
(export procedure? apply call-with-current-continuation call/cc values call-with-values vector-map vector-for-each
        map for-each string-map string-for-each)
(export make-parameter dynamic-wind)
(export port? input-port? output-port? current-input-port current-output-port current-error-port textual-port?
        binary-port? input-port-open? output-port-open? close-port close-input-port close-output-port open-output-string
        get-output-string open-output-bytevector get-output-bytevector open-input-string open-input-bytevector
        call-with-port)
(export eof-object? eof-object read-u8 peek-u8 read-char peek-char read-line read-bytevector read-string)
(export newline write-u8 write-char write-string write-bytevector flush-output-port)
(export features)
(export with-exception-handler raise raise-continuable error error-object? error-object-message error-object-irritants
        guard file-error? read-error?)
