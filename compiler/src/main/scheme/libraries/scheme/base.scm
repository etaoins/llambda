(define-library (scheme base)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))
  (import (llambda internal features))

  ; base library
  (include-library-declarations "../../interfaces/scheme/base.scm")
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
    
    ; Internal helper types
    (define-type <alist> (Listof <pair>))
    
    (define-r7rs eqv? (native-function "_lliby_is_eqv" (<any> <any>) -> <native-bool>))
    (define-r7rs eq? eqv?)
    (define-r7rs equal? (native-function "_lliby_is_equal" (<any> <any>) -> <native-bool>))

    (define-r7rs number? (make-predicate <number>))
    ; We only support real and rational numbers
    (define-r7rs complex? number?)
    (define-r7rs real? number?)

    (define-r7rs rational? (native-function "lliby_is_rational" (<any>) -> <native-bool>))
    
    ; These aren't quite normal predicates as they only take numbers
    (define-r7rs inexact? (lambda: ((val : <number>))
      ((make-predicate <flonum>) val)))

    (define-r7rs exact? (lambda: ((val : <number>))
      ((make-predicate <exact-integer>) val)))

    (define-r7rs exact-integer? exact?)

    (define-r7rs finite? (native-function "lliby_is_finite" (<number>) -> <native-bool>))
    (define-r7rs infinite? (native-function "lliby_is_infinite" (<number>) -> <native-bool>))
    (define-r7rs odd? (native-function "lliby_is_odd" (<native-int64>) -> <native-bool>))
    (define-r7rs even? (native-function "lliby_is_even" (<native-int64>) -> <native-bool>))

    (define-r7rs nan? (lambda: ((n : <number>))
      (eq? n +nan.0)))
    
    (define-r7rs = (native-function "lliby_numeric_equal" (<number> <number> . <number>) -> <native-bool>))
    (define-r7rs < (native-function "lliby_numeric_lt" (<number> <number> . <number>) -> <native-bool>))
    (define-r7rs > (native-function "lliby_numeric_gt" (<number> <number> . <number>) -> <native-bool>))
    (define-r7rs <= (native-function "lliby_numeric_lte" (<number> <number> . <number>) -> <native-bool>))
    (define-r7rs >= (native-function "lliby_numeric_gte" (<number> <number> . <number>) -> <native-bool>))

    ; These branch on type as our planner currently won't optimise comparisons without a definite type
    (define-r7rs zero? (lambda: ((n : <number>))
      (if (exact-integer? n) (= n 0) (= n 0.0))))

    (define-r7rs positive? (lambda: ((n : <number>))
      (if (exact-integer? n) (> n 0) (> n 0.0))))
    
    (define-r7rs negative? (lambda: ((n : <number>))
      (if (exact-integer? n) (< n 0) (< n 0.0))))
    
    (define native-floor (native-function "floor" (<native-double>) -> <native-double>))
    (define-r7rs floor (lambda: ((n : <number>))
      (if (exact-integer? n) n (native-floor n))))
    
    (define native-ceil (native-function "ceil" (<native-double>) -> <native-double>))
    (define-r7rs ceiling (lambda: ((n : <number>))
      (if (exact-integer? n) n (native-ceil n))))
    
    (define native-trunc (native-function "trunc" (<native-double>) -> <native-double>))
    (define-r7rs truncate (lambda: ((n : <number>))
      (if (exact-integer? n) n (native-trunc n))))
    
    (define native-round (native-function "round" (<native-double>) -> <native-double>))
    (define-r7rs round (lambda: ((n : <number>))
      (if (exact-integer? n) n (native-round n))))

    ; This is tricky because it's possible to be both inexact and an integer
    (define-r7rs integer? (lambda (x)
      (if (number? x)
        ; Some  numbers are integers
        (= x (floor x))
        ; Not numeric
        #f)))

    (define-r7rs exact (world-function "lliby_exact" (<number>) -> <native-int64>))
    (define-r7rs inexact (native-function "lliby_inexact" (<number>) -> <native-double>))

    (define-r7rs + (world-function "lliby_add" <number> -> <number>))
    (define-r7rs - (world-function "lliby_sub" (<number> . <number>) -> <number>))
    (define-r7rs * (world-function "lliby_mul" <number> -> <number>))
    (define-r7rs / (world-function "lliby_div" (<number> . <number>) -> <native-double>))
    
    (define-r7rs expt (world-function "lliby_expt" (<number> <number>) -> <number>))
    
    (define-r7rs square (lambda: ([num : <number>])
      (* num num)))

    (define-r7rs boolean? (make-predicate <boolean>))
    (define-r7rs not (make-predicate #f))
    (define-r7rs boolean=? (native-function "lliby_boolean_equal" (<boolean> <boolean> . <boolean>) -> <native-bool>))

    (define-r7rs pair? (make-predicate <pair>))
    (define-r7rs null? (make-predicate <empty-list>))
    (define-r7rs list? (make-predicate <list>))
    
    (define-r7rs length (world-function "lliby_length" (<list>) -> <native-uint32>))

    (define-r7rs cons (world-function "lliby_cons" (<any> <any>) -> <pair>))
    (define-r7rs car (native-function "lliby_car" (<pair>) -> <any>))
    (define-r7rs cdr (native-function "lliby_cdr" (<pair>) -> <any>))
    (define-r7rs caar (lambda: ((x : (Pairof <pair> <any>))) (car (car x))))
    (define-r7rs cadr (lambda: ((x : (Pairof <any> <pair>))) (car (cdr x))))
    (define-r7rs cdar (lambda: ((x : (Pairof <pair> <any>))) (cdr (car x))))
    (define-r7rs cddr (lambda: ((x : (Pairof <any> <pair>))) (cdr (cdr x))))

    (define-r7rs list-copy (world-function "lliby_list_copy" (<any>) -> <any>))
    (define-r7rs list (native-function "lliby_list" <any> -> <list>))
    (define-r7rs append (world-function "lliby_append" <any> -> <any>))

    (define-r7rs memv (native-function "lliby_memv" (<any> <list>) -> <any>))
    ; (eq?) is defined as (eqv?) so define (memq) as (memv)
    (define-r7rs memq memv)
    (define-r7rs member (native-function "lliby_member" (<any> <list>) -> <any>))
    
    (define-r7rs assv (native-function "lliby_assv" (<any> <alist>) -> <any>))
    (define-r7rs assq assv)
    (define-r7rs assoc (native-function "lliby_assoc" (<any> <alist>) -> <any>))

    (define-r7rs reverse (world-function "lliby_reverse" (<list>) -> <list>))

    (define-r7rs list-tail (world-function "lliby_list_tail" (<list> <native-uint32>) -> <list>))
    (define-r7rs list-ref (lambda: ([l : <list>] [n : <exact-integer>])
      (car (list-tail l n))))
    
    (define native-make-list (world-function "lliby_make_list" (<native-uint32> <any>) -> <list>))
    (define-r7rs make-list (case-lambda:
      (([len : <exact-integer>])
       (native-make-list len #!unit))
      (([len : <exact-integer>] [fill : <any>])
       (native-make-list len fill))))

    (define-r7rs symbol? (make-predicate <symbol>))
    (define-r7rs symbol=? (native-function "lliby_symbol_equal" (<symbol> <symbol> . <symbol>) -> <native-bool>))
    (define-r7rs symbol->string (world-function "lliby_symbol_to_string" (<symbol>) -> <string>))
    (define-r7rs string->symbol (world-function "lliby_string_to_symbol" (<string>) -> <symbol>))

    (define-r7rs char? (make-predicate <char>))
    (define-r7rs digit-value (world-function "lliby_digit_value" (<native-unicode-char>) -> <any>))
    (define-r7rs char->integer (native-function "lliby_char_to_integer" (<native-unicode-char>) -> <native-int32>))
    (define-r7rs integer->char (native-function "lliby_integer_to_char" (<native-int32>) -> <native-unicode-char>))

    (define-r7rs vector? (make-predicate <vector>))
    (define-r7rs vector (world-function "lliby_vector" <any> -> <vector>))
    ; This is the same runtime function but instead of using a rest arg explicitly pass in the list
    (define-r7rs list->vector (world-function "lliby_vector" (<list>) -> <vector>))
    (define-r7rs vector-length (native-function "lliby_vector_length" (<vector>) -> <native-uint32>))
    (define-r7rs vector-ref (world-function "lliby_vector_ref" (<vector> <native-uint32>) -> <any>))
    (define-r7rs vector-set! (world-function "lliby_vector_set" (<vector> <native-uint32> <any>)))
    (define-r7rs vector-append (world-function "lliby_vector_append" <vector> -> <vector>))
    
    (define native-make-vector (world-function "lliby_make_vector" (<native-uint32> <any>) -> <vector>))
    (define-r7rs make-vector (case-lambda:
      (([len : <exact-integer>])
       (native-make-vector len #!unit))
      (([len : <exact-integer>] [fill : <any>])
       (native-make-vector len fill))))

    (define-r7rs bytevector? (make-predicate <bytevector>))
    (define-r7rs bytevector (world-function "lliby_bytevector" <exact-integer> -> <bytevector>))
    (define-r7rs bytevector-length (native-function "lliby_bytevector_length" (<bytevector>) -> <native-uint32>))
    (define-r7rs bytevector-u8-ref (world-function "lliby_bytevector_u8_ref" (<bytevector> <native-uint32>) -> <native-uint8>))
    (define-r7rs bytevector-u8-set! (world-function "lliby_bytevector_u8_set" (<bytevector> <native-uint32> <native-uint8>)))
    (define-r7rs bytevector-append (world-function "lliby_bytevector_append" <bytevector> -> <bytevector>))
    
    (define native-make-bytevector (world-function "lliby_make_bytevector" (<native-uint32> <native-uint8>) -> <bytevector>))
    (define-r7rs make-bytevector (case-lambda:
      (([len : <exact-integer>])
       (native-make-bytevector len 0))
      (([len : <exact-integer>] [fill : <exact-integer>])
       (native-make-bytevector len fill))))

    (define-r7rs string? (make-predicate <string>))
    (define-r7rs make-string (world-function "lliby_make_string" (<native-uint32> <native-unicode-char>) -> <string>))
    (define-r7rs string (world-function "lliby_string" <char> -> <string>))
    ; This is the same runtime function but instead of using a rest arg explicitly pass in the list
    (define-r7rs list->string (world-function "lliby_string" (<list>) -> <string>))
    (define-r7rs string-length (native-function "lliby_string_length" (<string>) -> <native-uint32>))
    (define-r7rs string-ref (world-function "lliby_string_ref" (<string> <native-uint32>) -> <native-unicode-char>))
    (define-r7rs string-set! (world-function "lliby_string_set" (<string> <native-uint32> <native-unicode-char>)))
    (define-r7rs string-append (world-function "lliby_string_append" <string> -> <string>))

    (define-r7rs procedure? (make-predicate <procedure>))
    (define-r7rs call-with-current-continuation (world-function "lliby_call_with_current_continuation" ((-> <procedure> *)) -> *))
    (define-r7rs call/cc call-with-current-continuation)
    (define-r7rs values (native-function "lliby_values" <any> -> *))
    (define-r7rs call-with-values (world-function "lliby_call_with_values" ((-> *) <procedure>) -> *))
    (define-r7rs apply (world-function "lliby_apply" (<procedure> . <any>) -> *))

    (define native-make-parameter (world-function "_lliby_make_parameter" (<any> (U (-> <any> <any>) <unit>)) -> <procedure>))
    (define-r7rs make-parameter (case-lambda:
      (([init : <any>])
       (native-make-parameter init #!unit))
      (([init : <any>] [converter : (-> <any> <any>)])
       (native-make-parameter init converter))))

    (define-r7rs dynamic-wind (world-function "lliby_dynamic_wind" ((-> *) (-> *) (-> *)) -> *))

    ; Port support
    (define-r7rs port? (make-predicate <port>))
    (define-r7rs input-port? (native-function "lliby_is_input_port" (<any>) -> <native-bool>))
    (define-r7rs output-port? (native-function "lliby_is_output_port" (<any>) -> <native-bool>))

    (define-r7rs current-input-port (make-parameter ((world-function "_lliby_stdin_port" () -> <port>))))
    (define-r7rs current-output-port (make-parameter ((world-function "_lliby_stdout_port" () -> <port>))))
    (define-r7rs current-error-port (make-parameter ((world-function "_lliby_stderr_port" () -> <port>))))

    (define native-newline (world-function "lliby_newline" (<port>)))
    (define-r7rs newline (case-lambda:
      (()
       (native-newline (current-output-port)))
      (([port : <port>])
       (native-newline port))))

    (define-r7rs with-exception-handler (world-function "lliby_with_exception_handler" ((-> <any> *) (-> *)) -> *))
    (define-r7rs raise (world-function "lliby_raise" (<any>) noreturn))
    (define-r7rs raise-continuable (world-function "lliby_raise_continuable" (<any>) -> *))
    (define-r7rs error (world-function "lliby_error" (<string> . <any>) noreturn))
    (define-r7rs error-object? (make-predicate <error-object>))
    (define-r7rs error-object-message (native-function "lliby_error_object_message" (<error-object>) -> <string>))
    (define-r7rs error-object-irritants (native-function "lliby_error_object_irritants" (<error-object>) -> <list>))
    
    ; This is a native code helper which replaces most of the (guard) macro from R7RS with a much more efficient
    ; native code implementation
    (define guard-kernel (world-function "_lliby_guard_kernel" ((-> <any> *) (-> *)) -> *))

    (define-syntax guard
      (syntax-rules ()
                    ((guard (var clause ...) e1 e2 ...)
                     (guard-kernel
                       (lambda (var)
                         (guard-aux (raise var) clause ...))
                       (lambda () e1 e2 ...)))))

    ; This is taken directly from R7RS   
    (define-syntax guard-aux
      (syntax-rules (else =>)
                    ((guard-aux reraise (else result1 result2 ...))
                     (begin result1 result2 ...))
                    ((guard-aux reraise (test => result))
                     (let ((temp test))
                       (if temp
                         (result temp)
                         reraise)))
                    ((guard-aux reraise (test => result)
                                clause1 clause2 ...)
                     (let ((temp test))
                       (if temp
                         (result temp)
                         (guard-aux reraise clause1 clause2 ...))))
                    ((guard-aux reraise (test))
                     (or test reraise))
                    ((guard-aux reraise (test) clause1 clause2 ...)
                     (let ((temp test))
                       (if temp
                         temp
                         (guard-aux reraise clause1 clause2 ...))))
                    ((guard-aux reraise (test result1 result2 ...))
                     (if test
                       (begin result1 result2 ...)
                       reraise))
                    ((guard-aux reraise
                                (test result1 result2 ...)
                                clause1 clause2 ...)
                     (if test
                       (begin result1 result2 ...)
                       (guard-aux reraise clause1 clause2 ...))))))
    
  ; Optional R7RS mutable pair support
  (cond-expand ((not immutable-pairs)
    (begin
      (define-r7rs set-car! (world-function "lliby_set_car" (<pair> <any>)))
      (define-r7rs set-cdr! (world-function "lliby_set_cdr" (<pair> <any>)))
      (define-r7rs list-set! (lambda: ([l : <list>] [n : <exact-integer>] [val : <any>])
        (set-car! (list-tail l n) val))))))
)
