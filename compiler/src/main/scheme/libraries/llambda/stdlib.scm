(define-library (llambda stdlib)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))
  (import (llambda internal features))

  ; base library
  (include-library-declarations "../../interfaces/scheme/base.scm")
  ; case-lambda library 
  (include-library-declarations "../../interfaces/scheme/case-lambda.scm")
  (begin
    (define-syntax begin
      (syntax-rules ()
                    ((begin exp ...)
                     ((lambda () exp ...)))))

    ; This isn't the full definition - tagged let isn't supported
    (define-syntax let
      (syntax-rules ()
                    ((let ((name val) ...) body1 body2 ...)
                     ((lambda (name ...) body1 body2 ...)
                      val ...))))

    (define-syntax let*
      (syntax-rules ()
                    ((let* () body1 body2 ...)
                     (let () body1 body2 ...))
                    ((let* ((name1 val1) (name2 val2) ...)
                       body1 body2 ...)
                     (let ((name1 val1))
                       (let* ((name2 val2) ...)
                         body1 body2 ...)))))

    ; R7RS has a macro definition for (letrec*) that assigns locations a special "uninitialized" value and then
    ; implements multiple body (define)s in terms of (letrec*)
    ; Llambda takes the opposite approach of handling multiple body (define)s in the compiler frontend and then uses
    ; the macro below to implement (letrec*) in terms of body defines. The result should be equivalent.
    (define-syntax letrec*
      (syntax-rules ()
                    ((letrec* ((name val) ...) body1 body2 ...)
                     ((lambda ()
                        (define name val) ...
                        body1 body2 ...)))))

    ; XXX: This isn't quite right
    ; It's legal to refer to values earlier in the initializer list in (letrec*) but not in (letrec)
    ; This means all valid (letrec) initializers are valid (letrec*) initializers but not the converse. 
    ; We should be more strict with invalid (letrec) uses in the future
    (define-syntax letrec
      (syntax-rules ()
                    ((letrec ((name val) ...) body1 body2 ...)
                     (letrec* ((name val) ...) body1 body2 ...))))

    (define-syntax let
      (syntax-rules ()
                    ((let ((name val) ...) body1 body2 ...)
                     ((lambda (name ...) body1 body2 ...)
                      val ...))))

    (define-syntax letrec-syntax
      (syntax-rules ()
                    ((let-syntax ((name val) ...) body1 body2 ...)
                     ((lambda ()
                        (define-syntax name val) ...
                        body1 body2 ...)))))

    ; XXX: let-syntax is more restrictive than letrec-syntax
    (define-syntax let-syntax
      (syntax-rules ()
                    ((let-syntax ((name val) ...) body1 body2 ...)
                     (letrec-syntax ((name val) ...) body1 body2 ...))))

    (define-syntax cond
      (syntax-rules (else =>)
                    ((cond (else result1 result2 ...))
                     (begin result1 result2 ...))
                    ((cond (test => result))
                     (let ((temp test))
                       (if temp (result temp))))
                    ((cond (test => result) clause1 clause2 ...)
                     (let ((temp test))
                       (if temp
                         (result temp)
                         (cond clause1 clause2 ...))))
                    ((cond (test)) test)
                    ((cond (test) clause1 clause2 ...)
                     (let ((temp test))
                       (if temp temp
                         (cond clause1 clause2 ...))))
                    ((cond (test result1 result2 ...))
                     (if test (begin result1 result2 ...)))
                    ((cond (test result1 result2 ...)
                           clause1 clause2 ...)
                     (if test
                       (begin result1 result2 ...)
                       (cond clause1 clause2 ...)))))

    (define-syntax case
      (syntax-rules (else =>)
                    ((case (key ...)
                       clauses ...)
                     (let ((atom-key (key ...)))
                       (case atom-key clauses ...)))
                    ((case key
                       (else => result))
                     (result key))
                    ((case key
                       (else result1 result2 ...))
                     (begin result1 result2 ...))
                    ((case key
                       ((atoms ...) => result))
                     (if (memv key '(atoms ...))
                       (result key)))
                    ((case key
                       ((atoms ...) => result)
                       clause clauses ...)
                     (if (memv key '(atoms ...))
                       (result key)
                       (case key clause clauses ...)))
                    ((case key
                       ((atoms ...) result1 result2 ...))
                     (if (memv key '(atoms ...))
                       (begin result1 result2 ...)))
                    ((case key
                       ((atoms ...) result1 result2 ...)
                       clause clauses ...)
                     (if (memv key '(atoms ...))
                       (begin result1 result2 ...)
                       (case key clause clauses ...)))))

    (define-syntax and
      (syntax-rules ()
                    ((and) #t)
                    ((and test) test)
                    ((and test1 test2 ...)
                     (if test1 (and test2 ...) #f))))

    (define-syntax or
      (syntax-rules ()
                    ((or) #f)
                    ((or test) test)
                    ((or test1 test2 ...)
                     (let ((x test1))
                       (if x x (or test2 ...))))))

    (define-syntax when
      (syntax-rules ()
                    ((when test result1 result2 ...)
                     (if test
                       (begin result1 result2 ... #!unit)))))

    (define-syntax unless
      (syntax-rules ()
                    ((unless test result1 result2 ...)
                     (if (not test)
                       (begin result1 result2 ... #!unit)))))

    (define-syntax do
      (syntax-rules ()
                    ((do ((var init step ...) ...)
                       (test expr ...)
                       command ...)
                     (letrec
                       ((loop
                          (lambda (var ...)
                            (if test
                              (begin
                                (if #f #f)
                                expr ...)
                              (begin
                                command
                                ...
                                (loop (do "step" var step ...)
                                      ...))))))
                       (loop init ...)))
                    ((do "step" x)
                     x)
                    ((do "step" x y)
                     y)))
    
    ; Bootstrap definitions for case-lambda
    (define-r7rs apply (world-function "lliby_apply" (<procedure-cell> . <list-element-cell>) <datum-cell>))
    (define-r7rs length (world-function "lliby_length" (<list-element-cell>) <uint32>))
    (define-r7rs error (world-function "lliby_error" (<string-cell> . <list-element-cell>)))
    (define-r7rs = (world-function "lliby_numeric_equal" (<numeric-cell> <numeric-cell> . <list-element-cell>) <bool>))
    (define-r7rs < (world-function "lliby_numeric_lt" (<numeric-cell> <numeric-cell> . <list-element-cell>) <bool>))
    (define-r7rs > (world-function "lliby_numeric_gt" (<numeric-cell> <numeric-cell> . <list-element-cell>) <bool>))
    (define-r7rs <= (world-function "lliby_numeric_lte" (<numeric-cell> <numeric-cell> . <list-element-cell>) <bool>))
    (define-r7rs >= (world-function "lliby_numeric_gte" (<numeric-cell> <numeric-cell> . <list-element-cell>) <bool>))
    (define-r7rs positive? (native-function "lliby_is_positive" (<numeric-cell>) <bool>))
    (define-r7rs negative? (native-function "lliby_is_negative" (<numeric-cell>) <bool>))
    
    (define-syntax case-lambda
      (syntax-rules ()
                    ((case-lambda (params body0 ...) ...)
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
                           (cl (params body0 ...) ...)))))))

    (define-r7rs eqv? (native-function "lliby_is_eqv" (<datum-cell> <datum-cell>) <bool>))
    (define-r7rs eq? eqv?)
    (define-r7rs equal? (native-function "lliby_is_equal" (<datum-cell> <datum-cell>) <bool>))

    (define-r7rs number? (native-function "lliby_is_numeric" (<datum-cell>) <bool>))
    ; We only support real and rational numbers
    (define-r7rs complex? number?)
    (define-r7rs real? number?)
    (define-r7rs rational? number?)

    (define-r7rs integer? (native-function "lliby_is_exact_integer" (<datum-cell>) <bool>))

    (define-r7rs inexact? (native-function "lliby_is_inexact_rational" (<numeric-cell>) <bool>))
    ; This is the same as integer? with a different argument type
    (define-r7rs exact? (native-function "lliby_is_exact_integer" (<numeric-cell>) <bool>))
    (define-r7rs exact-integer? exact?)

    (define-r7rs finite? (native-function "lliby_is_finite" (<numeric-cell>) <bool>))
    (define-r7rs infinite? (native-function "lliby_is_infinite" (<numeric-cell>) <bool>))
    (define-r7rs nan? (native-function "lliby_is_nan" (<numeric-cell>) <bool>))
    (define-r7rs zero? (native-function "lliby_is_zero" (<numeric-cell>) <bool>))
    (define-r7rs odd? (native-function "lliby_is_odd" (<int64>) <bool>))
    (define-r7rs even? (native-function "lliby_is_even" (<int64>) <bool>))

    (define-r7rs exact (world-function "lliby_exact" (<numeric-cell>) <int64>))
    (define-r7rs inexact (world-function "lliby_inexact" (<numeric-cell>) <double>))

    (define-r7rs + (world-function "lliby_add" <list-element-cell> <numeric-cell>))
    (define-r7rs - (world-function "lliby_sub" (<numeric-cell> . <list-element-cell>) <numeric-cell>))
    (define-r7rs * (world-function "lliby_mul" <list-element-cell> <numeric-cell>))
    (define-r7rs / (world-function "lliby_div" (<numeric-cell> . <list-element-cell>) <double>))

    (define-r7rs boolean? (native-function "lliby_is_boolean" (<datum-cell>) <bool>))
    (define-r7rs not (native-function "lliby_not" (<bool>) <bool>))
    (define-r7rs boolean=? (world-function "lliby_boolean_equal" (<boolean-cell> <boolean-cell> . <list-element-cell>) <bool>))

    (define-r7rs pair? (native-function "lliby_is_pair" (<datum-cell>) <bool>))
    (define-r7rs null? (native-function "lliby_is_empty_list" (<datum-cell>) <bool>))
    (define-r7rs list? (native-function "lliby_is_list" (<datum-cell>) <bool>))
    (define-r7rs cons (world-function "lliby_cons" (<datum-cell> <datum-cell>) <pair-cell>))
    (define-r7rs car (native-function "lliby_car" (<pair-cell>) <datum-cell>))
    (define-r7rs cdr (native-function "lliby_cdr" (<pair-cell>) <datum-cell>))
    (define-r7rs set-car! (world-function "lliby_set_car" (<pair-cell> <datum-cell>)))
    (define-r7rs set-cdr! (world-function "lliby_set_cdr" (<pair-cell> <datum-cell>)))
    (define-r7rs list-copy (world-function "lliby_list_copy" (<datum-cell>) <datum-cell>))
    (define-r7rs list (native-function "lliby_list" <list-element-cell> <list-element-cell>))
    (define-r7rs append (world-function "lliby_append" <list-element-cell> <datum-cell>))
    (define-r7rs memv (world-function "lliby_memv" (<datum-cell> <list-element-cell>) <datum-cell>))
    ; (eq?) is defined as (eqv?) so define (memq) as (memv)
    (define-r7rs memq memv)
    (define-r7rs member (world-function "lliby_member" (<datum-cell> <list-element-cell>) <datum-cell>))
    
    (define native-make-list (world-function "lliby_make_list" (<uint32> <datum-cell>) <list-element-cell>))
    (define-r7rs make-list (case-lambda
      ((len) (native-make-list len #!unit))
      ((len fill) (native-make-list len fill))))

    (define-r7rs symbol? (native-function "lliby_is_symbol" (<datum-cell>) <bool>))
    (define-r7rs symbol=? (world-function "lliby_symbol_equal" (<symbol-cell> <symbol-cell> . <list-element-cell>) <bool>))
    (define-r7rs symbol->string (world-function "lliby_symbol_to_string" (<symbol-cell>) <string-cell>))
    (define-r7rs string->symbol (world-function "lliby_string_to_symbol" (<string-cell>) <symbol-cell>))

    (define-r7rs char? (native-function "lliby_is_character" (<datum-cell>) <bool>))
    (define-r7rs digit-value (world-function "lliby_digit_value" (<unicode-char>) <datum-cell>))
    (define-r7rs char->integer (native-function "lliby_char_to_integer" (<unicode-char>) <int32>))
    (define-r7rs integer->char (native-function "lliby_integer_to_char" (<int32>) <unicode-char>))

    (define-r7rs vector? (native-function "lliby_is_vector" (<datum-cell>) <bool>))
    (define-r7rs vector (world-function "lliby_vector" <list-element-cell> <vector-cell>))
    ; This is the same runtime function but instead of using a rest arg explicitly pass in the list
    (define-r7rs list->vector (world-function "lliby_vector" (<list-element-cell>) <vector-cell>))
    (define-r7rs vector-length (native-function "lliby_vector_length" (<vector-cell>) <uint32>))
    (define-r7rs vector-ref (world-function "lliby_vector_ref" (<vector-cell> <uint32>) <datum-cell>))
    (define-r7rs vector-set! (world-function "lliby_vector_set" (<vector-cell> <uint32> <datum-cell>)))
    (define-r7rs vector-append (world-function "lliby_vector_append" <list-element-cell> <vector-cell>))
    
    (define native-make-vector (world-function "lliby_make_vector" (<uint32> <datum-cell>) <vector-cell>))
    (define-r7rs make-vector (case-lambda
      ((len) (native-make-vector len #!unit))
      ((len fill) (native-make-vector len fill))))

    (define-r7rs bytevector? (native-function "lliby_is_bytevector" (<datum-cell>) <bool>))
    (define-r7rs bytevector (world-function "lliby_bytevector" <list-element-cell> <bytevector-cell>))
    (define-r7rs bytevector-length (native-function "lliby_bytevector_length" (<bytevector-cell>) <uint32>))
    (define-r7rs bytevector-u8-ref (world-function "lliby_bytevector_u8_ref" (<bytevector-cell> <uint32>) <uint8>))
    (define-r7rs bytevector-u8-set! (world-function "lliby_bytevector_u8_set" (<bytevector-cell> <uint32> <uint8>)))
    (define-r7rs bytevector-append (world-function "lliby_bytevector_append" <list-element-cell> <bytevector-cell>))
    
    (define native-make-bytevector (world-function "lliby_make_bytevector" (<uint32> <uint8>) <bytevector-cell>))
    (define-r7rs make-bytevector (case-lambda
      ((len) (native-make-bytevector len 0))
      ((len fill) (native-make-bytevector len fill))))

    (define-r7rs string? (native-function "lliby_is_string" (<datum-cell>) <bool>))
    (define-r7rs make-string (world-function "lliby_make_string" (<uint32> <unicode-char>) <string-cell>))
    (define-r7rs string (world-function "lliby_string" <list-element-cell> <string-cell>))
    ; This is the same runtime function but instead of using a rest arg explicitly pass in the list
    (define-r7rs list->string (world-function "lliby_string" (<list-element-cell>) <string-cell>))
    (define-r7rs string-length (native-function "lliby_string_length" (<string-cell>) <uint32>))
    (define-r7rs string-ref (world-function "lliby_string_ref" (<string-cell> <uint32>) <unicode-char>))
    (define-r7rs string-set! (world-function "lliby_string_set" (<string-cell> <uint32> <unicode-char>)))
    (define-r7rs string-append (world-function "lliby_string_append" <list-element-cell> <string-cell>))

    (define-r7rs procedure? (native-function "lliby_is_procedure" (<datum-cell>) <bool>))
    (define-r7rs call-with-current-continuation (world-function "lliby_call_with_current_continuation" (<datum-cell>) <datum-cell>))
    (define-r7rs call/cc call-with-current-continuation)

    ; XXX: This should accept a procedure once (case-lambda) is implemented
    (define-r7rs make-parameter (world-function "lliby_make_parameter" (<datum-cell>) <procedure-cell>))
    (define-r7rs dynamic-wind (world-function "lliby_dynamic_wind" (<procedure-cell> <procedure-cell> <procedure-cell>) <datum-cell>))

    ; Port support
    (define-r7rs port? (native-function "lliby_is_port" (<datum-cell>) <bool>))
    (define-r7rs input-port? (native-function "lliby_is_input_port" (<datum-cell>) <bool>))
    (define-r7rs output-port? (native-function "lliby_is_output_port" (<datum-cell>) <bool>))

    (define-r7rs current-input-port (make-parameter ((world-function "_lliby_stdin_port" () <port-cell>))))
    (define-r7rs current-output-port (make-parameter ((world-function "_lliby_stdout_port" () <port-cell>))))
    (define-r7rs current-error-port (make-parameter ((world-function "_lliby_stderr_port" () <port-cell>))))

    (define native-newline (world-function "lliby_newline" (<port-cell>)))
    (define-r7rs newline (case-lambda
      (() (native-newline (current-output-port)))
      ((port) (native-newline port))))

    (define-r7rs with-exception-handler (world-function "lliby_with_exception_handler" (<procedure-cell> <procedure-cell>) <datum-cell>))
    (define-r7rs raise (world-function "lliby_raise" (<datum-cell>)))
    (define-r7rs error-object? (native-function "lliby_is_error_object" (<datum-cell>) <bool>))
    (define-r7rs error-object-message (native-function "lliby_error_object_message" (<error-object-cell>) <string-cell>))
    (define-r7rs error-object-irritants (native-function "lliby_error_object_irritants" (<error-object-cell>) <list-element-cell>)))

  ; process-context library
  (include-library-declarations "../../interfaces/scheme/process-context.scm")
  (begin
    (define-r7rs exit (world-function "lliby_exit" (<datum-cell>)))
    (define-r7rs emergency-exit (native-function "lliby_emergency_exit" (<datum-cell>))))

  ; inexact library
  (include-library-declarations "../../interfaces/scheme/inexact.scm")
  (begin
    ; These always return inexact numbers so we can use the C standard
    ; library directly
    (define-r7rs sin (native-function "sin" (<double>) <double>))
    (define-r7rs cos (native-function "cos" (<double>) <double>))
    (define-r7rs tan (native-function "tan" (<double>) <double>)))

  ; write library
  (include-library-declarations "../../interfaces/scheme/write.scm")
  (begin
    (define native-write (world-function "lliby_write" (<datum-cell> <port-cell>)))
    (define-r7rs write (case-lambda
      ((datum) (native-write datum (current-output-port)))
      ((datum port) (native-write datum port))))

    (define native-display (world-function "lliby_display" (<datum-cell> <port-cell>)))
    (define-r7rs display (case-lambda
      ((datum) (native-display datum (current-output-port)))
      ((datum port) (native-display datum port)))))
  )
