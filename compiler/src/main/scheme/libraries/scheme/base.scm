(define-library (scheme base)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))
  (import (llambda internal features))
  (import (llambda error))

  ; base library
  (include-library-declarations "../../interfaces/scheme/base.scm")
  (begin
    (define-syntax =>
      (syntax-rules ()))

    (define-syntax else
      (syntax-rules ()))

    (define-syntax let
      (syntax-rules ()
                    ((let ((name val) ...) body1 body2 ...)
                     ((lambda (name ...) body1 body2 ...)
                      val ...))
                    ((let tag ((name val) ...) body1 body2 ...)
                     ((letrec ((tag (lambda (name ...)
                                      body1 body2 ...)))
                        tag)
                      val ...))
                    ((let ((name : <type> val) ...) body1 body2 ...)
                     ((lambda ((name : <type>) ...) body1 body2 ...)
                      val ...))
                    ((let tag ((name : <type> val) ...) body1 body2 ...)
                     ((letrec ((tag (lambda ([name : <type>] ...)
                                      body1 body2 ...)))
                        tag)
                      val ...))))

    (define-syntax let*
      (syntax-rules ()
                    ((let* () body1 body2 ...)
                     (let () body1 body2 ...))
                    ((let* ([name1 : <type1> val1] [name2 : <type2> val2] ...)
                       body1 body2 ...)
                     (let ([name1 : <type1> val1])
                       (let* ([name2 : <type2> val2] ...)
                         body1 body2 ...)))
                    ((let* ((name1 val1) (name2 val2) ...)
                       body1 body2 ...)
                     (let ((name1 val1))
                       (let* ((name2 val2) ...)
                         body1 body2 ...)))))

    #| R7RS has a macro definition for (letrec*) that assigns locations a special "uninitialized" value and then
       implements multiple body (define)s in terms of (letrec*)
       Llambda takes the opposite approach of handling multiple body (define)s in the compiler frontend and then uses
       the macro below to implement (letrec*) in terms of body defines. The result should be equivalent. |#
    (define-syntax letrec*
      (syntax-rules ()
                    ((letrec* ((name : <type> val) ...) body1 body2 ...)
                     ((lambda ()
                        (define name : <type> val) ...
                        body1 body2 ...)))
                    ((letrec* ((name val) ...) body1 body2 ...)
                     ((lambda ()
                        (define name val) ...
                        body1 body2 ...)))))

    #| XXX: This isn't quite right
       It's legal to refer to values earlier in the initializer list in (letrec*) but not in (letrec)
       This means all valid (letrec) initializers are valid (letrec*) initializers but not the converse.
       We should be more strict with invalid (letrec) uses in the future |#
    (define-syntax letrec
      (syntax-rules ()
                    ((letrec ((name : <type> val) ...) body1 body2 ...)
                     (letrec* ((name : <type> val) ...) body1 body2 ...))
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
                    ((do (([var : <type>] init step ...) ...)
                       (test expr ...)
                       command ...)
                     (letrec
                       ((loop
                          (lambda ([var : <type>] ...)
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
                    ((do ((var init step ...) ...)
                       (test expr ...)
                       command ...)
                     (do (([var : <any>] init step ...) ...)
                       (test expr ...)
                       command ...))
                    ((do "step" x)
                     x)
                    ((do "step" x y)
                     y)))

    ; Internal helper types
    (define-type (Alistof A B) (Listof (Pairof A B)))

    #| Defines a procedure that operates on a slice of a given type with optional start and end indicies
       native-proc should accept a value of <source-type> and a start and end index
       source-length-proc should accept a value of <source-type> and return its length |#
    (define-syntax define-slice-proc
      (syntax-rules ()
        ((define-slice-proc name native-proc <source-type> source-length-proc)
         (define-r7rs (name [source : <source-type>] [start : <exact-integer> 0] [end : <exact-integer> (source-length-proc source)])
           (native-proc source start end)))))

    (define-syntax define-mutating-copy-proc
      (syntax-rules ()
        ((define-mutating-copy-proc name native-proc <type> length-proc)
         (define-r7rs (name [to : <type>] [at : <exact-integer>] [from : <type>] [start : <exact-integer> 0] [end : <exact-integer> (length-proc from)])
           (native-proc to at from start end)))))

    (define-native-library llbase (static-library "ll_scheme_base"))

    ; Define the length accessors for slicing values
    (define-r7rs vector-length (native-function llbase "llbase_vector_length" (-> <vector> <native-int64>)))
    (define-r7rs bytevector-length (native-function llbase "llbase_bytevector_length" (-> <bytevector> <native-int64>)))
    (define-r7rs string-length (native-function llbase "llbase_string_length" (-> <string> <native-uint32>)))

    (define-r7rs eqv? (native-function system-library "llcore_is_eqv" (-> <any> <any> <native-bool>)))
    (define-r7rs equal? (native-function system-library "llcore_is_equal" (-> <any> <any> <native-bool>)))

    (define-r7rs boolean? (make-predicate <boolean>))
    (define-r7rs not (make-predicate #f))
    (define-r7rs boolean=? (native-function llbase "llbase_boolean_equal" (-> <boolean> <boolean> <boolean> * <native-bool>)))

    (define-r7rs procedure? (make-predicate <procedure>))
    (define-r7rs apply (world-function llbase "llbase_apply" (-> <procedure> <any> * <any>)))

    (define-r7rs number? (make-predicate <number>))

    (define-r7rs rational? (native-function llbase "llbase_is_rational" (-> <any> <native-bool>)))

    ; These aren't quite normal predicates as they only take numbers
    (define-r7rs (inexact? [val : <number>])
      ((make-predicate <flonum>) val))

    (define-r7rs (exact? [val : <number>])
      ((make-predicate <exact-integer>) val))

    (define-r7rs exact-integer? exact?)


    (define-r7rs = (native-function llbase "llbase_numeric_equal" (-> <number> <number> <number> * <native-bool>)))
    (define-r7rs < (native-function llbase "llbase_numeric_lt" (-> <number> <number> <number> * <native-bool>)))
    (define-r7rs > (native-function llbase "llbase_numeric_gt" (-> <number> <number> <number> * <native-bool>)))
    (define-r7rs <= (native-function llbase "llbase_numeric_lte" (-> <number> <number> <number> * <native-bool>)))
    (define-r7rs >= (native-function llbase "llbase_numeric_gte" (-> <number> <number> <number> * <native-bool>)))

    ; These branch on type as our planner currently won't optimise comparisons without a definite type
    (define-r7rs (zero? [n : <number>])
      (if (exact-integer? n) (= n 0) (= n 0.0)))

    (define-r7rs (positive? [n : <number>])
      (if (exact-integer? n) (> n 0) (> n 0.0)))

    (define-r7rs (negative? [n : <number>])
      (if (exact-integer? n) (< n 0) (< n 0.0)))

    (define native-floor (native-function system-library "floor" (-> <native-double> <native-double>)))
    (: floor (All ([N : <number>]) (-> N N)))
    (define-r7rs (floor n)
      (if (exact-integer? n) n (native-floor n)))

    (define native-ceil (native-function system-library "ceil" (-> <native-double> <native-double>)))
    (: ceiling (All ([N : <number>]) (-> N N)))
    (define-r7rs (ceiling n)
      (if (exact-integer? n) n (native-ceil n)))

    (define native-trunc (native-function system-library "trunc" (-> <native-double> <native-double>)))
    (: truncate (All ([N : <number>]) (-> N N)))
    (define-r7rs (truncate n)
      (if (exact-integer? n) n (native-trunc n)))

    (define native-round (native-function system-library "round" (-> <native-double> <native-double>)))
    (: round (All ([N : <number>]) (-> N N)))
    (define-r7rs (round n)
      (if (exact-integer? n) n (native-round n)))

    (define-r7rs (integer? x)
      (if (number? x)
        (if (exact-integer? x)
          ; All exact integers are integers
          #t
          ; Some flonums are integers
          (= x (floor x)))
        ; Not numeric
        #f))

    (define-r7rs exact (world-function llbase "llbase_exact" (-> <number> <native-int64>)))
    (define-r7rs inexact (native-function llbase "llbase_inexact" (-> <number> <native-double>)))

    (define-r7rs + (world-function llbase "llbase_add" (All ([N : <number>]) N * N)))
    (define-r7rs - (world-function llbase "llbase_sub" (All ([N : <number>]) N N * N)))
    (define-r7rs * (world-function llbase "llbase_mul" (All ([N : <number>]) N * N)))
    (define-r7rs / (world-function llbase "llbase_div" (-> <number> <number> * <number>)))

    (define-r7rs expt (world-function llbase "llbase_expt" (All ([N : <number>]) (-> N N N))))

    (: square (All ([N : <number>]) (-> N N)))
    (define-r7rs (square num)
      (* num num))

    (: abs (All ([N : <number>]) (-> N N)))
    (define-r7rs (abs num)
      ; Do a top-level type check to make the compiler generate a specialised version of each branch. The test itself is
      ; semantically a no-op
      (if (exact-integer? num)
        (if (< num 0)
          (- num)
          num)
        #| This generates less efficient code than fabs()
           However, this has two important benefits: it can be statically evaluated without any explicit (abs) planning
           in the compiler and it avoid a cell allocation for positive values. |#
        (if (zero? num)
          0.0
          (if (< num 0.0)
            (- num)
            num))))

    (define-r7rs truncate/ (world-function llbase "llbase_truncate_div" (-> <native-int64> <native-int64> (Pairof <exact-integer> <exact-integer>))))
    (define-r7rs truncate-quotient (world-function llbase "llbase_truncate_quotient" (-> <native-int64> <native-int64> <native-int64>)))
    (define-r7rs truncate-remainder (world-function llbase "llbase_truncate_remainder" (-> <native-int64> <native-int64> <native-int64>)))

    (define-r7rs floor/ (world-function llbase "llbase_floor_div" (-> <native-int64> <native-int64> (Pairof <exact-integer> <exact-integer>))))
    (define-r7rs floor-quotient (world-function llbase "llbase_floor_quotient" (-> <native-int64> <native-int64> <native-int64>)))
    (define-r7rs floor-remainder (world-function llbase "llbase_floor_remainder" (-> <native-int64> <native-int64> <native-int64>)))

    (define-r7rs (odd? [val : <exact-integer>])
                 (not (= (truncate-remainder val 2) 0)))

    (define-r7rs (even? [val : <exact-integer>])
                 (= (truncate-remainder val 2) 0))

    (define-r7rs max (world-function llbase "llbase_max" (All ([N : <number>]) N N * N)))
    (define-r7rs min (world-function llbase "llbase_min" (All ([N : <number>]) N N * N)))

    (define native-gcd (native-function llbase "llbase_gcd" (-> <native-int64> <native-int64> <exact-integer> * <native-int64>)))
    (define-r7rs gcd (case-lambda
                       (() 0)
                       (([single : <exact-integer>]) (abs single))
                       (rest (apply native-gcd rest))))

    (define native-lcm (native-function llbase "llbase_lcm" (-> <native-int64> <native-int64> <exact-integer> * <native-int64>)))
    (define-r7rs lcm (case-lambda
                       (() 1)
                       (([single : <exact-integer>]) (abs single))
                       (rest (apply native-lcm rest))))

    (define-r7rs exact-integer-sqrt (world-function llbase "llbase_exact_integer_sqrt" (-> <native-int64> (Pairof <exact-integer> <exact-integer>))))

    (define native-number->string (world-function llbase "llbase_number_to_string" (-> <number> <native-uint8> <string>)))
    (define-r7rs (number->string [num : <number>] [radix : <exact-integer> 10])
      (native-number->string num radix))

    (define native-string->number (world-function llbase "llbase_string_to_number" (-> <string> <native-uint8> (U #f <number>))))
    (define-r7rs (string->number [str : <string>] [radix : <exact-integer> 10])
      (native-string->number str radix))

    (define-r7rs pair? (make-predicate <pair>))
    (define-r7rs null? (make-predicate <empty-list>))
    (define-r7rs list? (make-predicate <list>))

    (define-r7rs length (native-function llbase "llbase_length" (-> <list> <native-uint32>)))

    (define-r7rs cons (world-function llbase "llbase_cons" (All (A B) A B (Pairof A B))))
    (define-r7rs car (native-function llbase "llbase_car" (All (A) (Pairof A <any>) A)))
    (define-r7rs cdr (native-function llbase "llbase_cdr" (All (A) (Pairof <any> A) A)))
    (define-r7rs (caar (x : (Pairof <pair> <any>))) (car (car x)))
    (define-r7rs (cadr (x : (Pairof <any> <pair>))) (car (cdr x)))
    (define-r7rs (cdar (x : (Pairof <pair> <any>))) (cdr (car x)))
    (define-r7rs (cddr (x : (Pairof <any> <pair>))) (cdr (cdr x)))

    (: list (All (A) (-> A * (Listof A))))
    (define-r7rs (list . rest) rest)

    (define-r7rs append (world-function llbase "llbase_append" (-> <any> * <any>)))

    (define-r7rs memv (native-function llbase "llbase_memv" (All (A) A (Listof A) (U (Listof A) #f))))
    (define-r7rs member (native-function llbase "llbase_member" (All (A) A (Listof A) (U (Listof A) #f))))

    (define-r7rs assv (native-function llbase "llbase_assv" (All (A B) A (Alistof A B) (U (Pairof A B) #f))))
    (define-r7rs assoc (native-function llbase "llbase_assoc" (All (A B) A (Alistof A B) (U (Pairof A B) #f))))

    (define-r7rs reverse (world-function llbase "llbase_reverse" (All (A) (Listof A) (Listof A))))

    (define-r7rs list-tail (world-function llbase "llbase_list_tail" (All (A) (Listof A) <native-uint32> (Listof A))))

    (: list-ref (All (A) (-> (Listof A) <exact-integer> A)))
    (define-r7rs (list-ref l n)
      (define tail (list-tail l n))
      (if (pair? tail)
        (car tail)
        (raise-range-error "(list-ref) at exact end of list" n)))

    (define native-make-list (world-function llbase "llbase_make_list" (All (A) <native-uint32> A (Listof A))))
    (define-r7rs (make-list [len : <exact-integer>] [fill : <any> #!unit])
      (native-make-list len fill))

    (define-r7rs symbol? (make-predicate <symbol>))
    (define-r7rs symbol=? (native-function llbase "llbase_symbol_equal" (-> <symbol> <symbol> <symbol> * <native-bool>)))
    (define-r7rs symbol->string (world-function llbase "llbase_symbol_to_string" (-> <symbol> <string>)))
    (define-r7rs string->symbol (world-function llbase "llbase_string_to_symbol" (-> <string> <symbol>)))

    (define-r7rs char? (make-predicate <char>))
    (define-r7rs char->integer (native-function llbase "llbase_char_to_integer" (-> <native-unicode-char> <native-int32>)))
    (define-r7rs integer->char (world-function llbase "llbase_integer_to_char" (-> <native-int64> <native-unicode-char>)))
    (define-r7rs char=? (native-function llbase "llbase_char_equal" (-> <native-unicode-char> <native-unicode-char> <char> * <native-bool>)))
    (define-r7rs char<? (native-function llbase "llbase_char_lt" (-> <native-unicode-char> <native-unicode-char> <char> * <native-bool>)))
    (define-r7rs char>? (native-function llbase "llbase_char_gt" (-> <native-unicode-char> <native-unicode-char> <char> * <native-bool>)))
    (define-r7rs char<=? (native-function llbase "llbase_char_lte" (-> <native-unicode-char> <native-unicode-char> <char> * <native-bool>)))
    (define-r7rs char>=? (native-function llbase "llbase_char_gte" (-> <native-unicode-char> <native-unicode-char> <char> * <native-bool>)))

    (define-r7rs vector? (make-predicate <vector>))
    (define-r7rs vector (world-function llbase "llbase_vector" (-> <any> * <vector>)))
    ; This is the same runtime function but instead of using a rest arg explicitly pass in the list
    (define-r7rs list->vector (world-function llbase "llbase_vector" (-> <list> <vector>)))
    (define-r7rs vector-ref (world-function llbase "llbase_vector_ref" (-> <vector> <native-int64> <any>)))
    (define-r7rs vector-set! (world-function llbase "llbase_vector_set" (-> <vector> <native-int64> <any> <unit>)))
    (define-r7rs vector-append (world-function llbase "llbase_vector_append" (-> <vector> * <vector>)))

    (define native-vector->list (world-function llbase "llbase_vector_to_list" (-> <vector> <native-int64> <native-int64> <list>)))
    (define-slice-proc vector->list native-vector->list <vector> vector-length)

    (define native-vector-copy (world-function llbase "llbase_vector_copy" (-> <vector> <native-int64> <native-int64> <vector>)))
    (define-slice-proc vector-copy native-vector-copy <vector> vector-length)

    (define native-vector-copy! (world-function llbase "llbase_vector_mutating_copy" (-> <vector> <native-int64> <vector> <native-int64> <native-int64> <unit>)))
    (define-mutating-copy-proc vector-copy! native-vector-copy! <vector> vector-length)

    (define native-vector-fill! (world-function llbase "llbase_vector_mutating_fill" (-> <vector> <any> <native-int64> <native-int64> <unit>)))
    (define-r7rs (vector-fill! [target : <vector>] [fill : <any>] [start : <exact-integer> 0] [end : <exact-integer> (vector-length target)])
      (native-vector-fill! target fill start end))

    (define native-make-vector (world-function llbase "llbase_make_vector" (-> <native-int64> <any> <vector>)))
    (define-r7rs (make-vector [len : <exact-integer>] [fill : <any> #!unit])
      (native-make-vector len fill))

    (define native-vector->string (world-function llbase "llbase_vector_to_string" (-> <vector> <native-int64> <native-int64> <string>)))
    (define-slice-proc vector->string native-vector->string <vector> vector-length)

    (define native-string->vector (world-function llbase "llbase_string_to_vector" (-> <string> <native-int64> <native-int64> <vector>)))
    (define-slice-proc string->vector native-string->vector <string> string-length)

    (define-r7rs bytevector? (make-predicate <bytevector>))
    (define-r7rs bytevector (world-function llbase "llbase_bytevector" (-> <exact-integer> * <bytevector>)))
    (define-r7rs bytevector-u8-ref (world-function llbase "llbase_bytevector_u8_ref" (-> <bytevector> <native-int64> <native-uint8>)))
    (define-r7rs bytevector-u8-set! (world-function llbase "llbase_bytevector_u8_set" (-> <bytevector> <native-int64> <native-uint8> <unit>)))
    (define-r7rs bytevector-append (world-function llbase "llbase_bytevector_append" (-> <bytevector> * <bytevector>)))

    (define native-bytevector-copy (world-function llbase "llbase_bytevector_copy" (-> <bytevector> <native-int64> <native-int64> <bytevector>)))
    (define-slice-proc bytevector-copy native-bytevector-copy <bytevector> bytevector-length)

    (define native-bytevector-copy! (world-function llbase "llbase_bytevector_mutating_copy" (-> <bytevector> <native-int64> <bytevector> <native-int64> <native-int64> <unit>)))
    (define-mutating-copy-proc bytevector-copy! native-bytevector-copy! <bytevector> bytevector-length)

    (define native-utf8->string (world-function llbase "llbase_utf8_to_string" (-> <bytevector> <native-int64> <native-int64> <string>)))
    (define-slice-proc utf8->string native-utf8->string <bytevector> bytevector-length)

    (define native-make-bytevector (world-function llbase "llbase_make_bytevector" (-> <native-int64> <native-uint8> <bytevector>)))
    (define-r7rs (make-bytevector [len : <exact-integer>] [fill : <exact-integer> 0])
      (native-make-bytevector len fill))

    (define-r7rs string? (make-predicate <string>))
    (define-r7rs make-string (world-function llbase "llbase_make_string" (-> <native-int64> <native-unicode-char> <string>)))
    (define-r7rs string (world-function llbase "llbase_string" (-> <char> * <string>)))
    ; This is the same runtime function but instead of using a rest arg explicitly pass in the list
    (define-r7rs list->string (world-function llbase "llbase_string" (-> (Listof <char>) <string>)))
    (define-r7rs string-ref (world-function llbase "llbase_string_ref" (-> <string> <native-int64> <native-unicode-char>)))
    (define-r7rs string-set! (world-function llbase "llbase_string_set" (-> <string> <native-int64> <native-unicode-char> <unit>)))
    (define-r7rs string-append (world-function llbase "llbase_string_append" (-> <string> * <string>)))

    (define native-string->list (world-function llbase "llbase_string_to_list" (-> <string> <native-int64> <native-int64> (Listof <char>))))
    (define-slice-proc string->list native-string->list <string> string-length)

    (define native-string->utf8 (world-function llbase "llbase_string_to_utf8" (-> <string> <native-int64> <native-int64> <bytevector>)))
    (define-slice-proc string->utf8 native-string->utf8 <string> string-length)

    #| Unlike other slicing functions the raw slicer is exposed as (substring) to implement the procedure with the same
       name defined in R7RS |#
    (define-r7rs substring (world-function llbase "llbase_substring" (-> <string> <native-int64> <native-int64> <string>)))
    (define-slice-proc string-copy substring <string> string-length)

    (define native-string-copy! (world-function llbase "llbase_string_mutating_copy" (-> <string> <native-int64> <string> <native-int64> <native-int64> <unit>)))
    (define-mutating-copy-proc string-copy! native-string-copy! <string> string-length)

    (define native-string-fill! (world-function llbase "llbase_string_mutating_fill" (-> <string> <native-unicode-char> <native-int64> <native-int64> <unit>)))
    (define-r7rs (string-fill! [target : <string>] [fill : <char>] [start : <exact-integer> 0] [end : <exact-integer> (string-length target)])
      (native-string-fill! target fill start end))

    (define-r7rs string=? (native-function llbase "llbase_string_equal" (-> <string> <string> <string> * <native-bool>)))
    (define-r7rs string<? (native-function llbase "llbase_string_lt" (-> <string> <string> <string> * <native-bool>)))
    (define-r7rs string>? (native-function llbase "llbase_string_gt" (-> <string> <string> <string> * <native-bool>)))
    (define-r7rs string<=? (native-function llbase "llbase_string_lte" (-> <string> <string> <string> * <native-bool>)))
    (define-r7rs string>=? (native-function llbase "llbase_string_gte" (-> <string> <string> <string> * <native-bool>)))

    (define-r7rs vector-map (world-function llbase "llbase_vector_map" (-> (-> <any> <any> * <any>) <vector> <vector> * <vector>)))
    (define-r7rs vector-for-each (world-function llbase "llbase_vector_for_each" (-> (-> <any> <any> * <unit>) <vector> <vector> * <unit>)))

    (define-r7rs map (world-function llbase "llbase_map" (All (A) (-> <any> <any> * A) <list> <list> * (Listof A))))
    (define-r7rs for-each (world-function llbase "llbase_for_each" (-> (-> <any> <any> * <unit>) <list> <list> * <unit>)))

    (define-r7rs string-map (world-function llbase "llbase_string_map" (-> (-> <char> <char> * <char>) <string> <string> * <string>)))
    (define-r7rs string-for-each (world-function llbase "llbase_string_for_each" (-> (-> <char> <char> * <unit>) <string> <string> * <unit>)))

    (define native-make-parameter (world-function system-library "llcore_make_parameter" (-> <any> (U (-> <any> <any>) <unit>) <procedure>)))
    (define-r7rs make-parameter (case-lambda
      (([init : <any>])
       (native-make-parameter init #!unit))
      (([init : <any>] [converter : (-> <any> <any>)])
       (native-make-parameter init converter))))

    ; Port support
    (define-r7rs port? (make-predicate <port>))
    (define-r7rs input-port? (native-function llbase "llbase_is_input_port" (-> <any> <native-bool>)))
    (define-r7rs output-port? (native-function llbase "llbase_is_output_port" (-> <any> <native-bool>)))
    (define-r7rs textual-port? port?)
    (define-r7rs binary-port? port?)
    (define-r7rs input-port-open? (native-function llbase "llbase_is_input_port_open" (-> <port> <native-bool>)))
    (define-r7rs output-port-open? (native-function llbase "llbase_is_output_port_open" (-> <port> <native-bool>)))
    (define-r7rs close-port (native-function llbase "llbase_close_port" (-> <port> <unit>)))
    (define-r7rs close-input-port (world-function llbase "llbase_close_input_port" (-> <port> <unit>)))
    (define-r7rs close-output-port (world-function llbase "llbase_close_output_port" (-> <port> <unit>)))
    (define-r7rs open-output-string (world-function llbase "llbase_open_output_string" (-> <port>)))
    (define-r7rs get-output-string (world-function llbase "llbase_get_output_string" (-> <port> <string>)))
    (define-r7rs open-output-bytevector (world-function llbase "llbase_open_output_bytevector" (-> <port>)))
    (define-r7rs get-output-bytevector (world-function llbase "llbase_get_output_bytevector" (-> <port> <bytevector>)))
    (define-r7rs open-input-string (world-function llbase "llbase_open_input_string" (-> <string> <port>)))
    (define-r7rs open-input-bytevector (world-function llbase "llbase_open_input_bytevector" (-> <bytevector> <port>)))
    (define-r7rs call-with-port (world-function llbase "llbase_call_with_port" (-> <port> (-> <port> <any>) <any>)))

    (define-r7rs current-input-port (make-parameter ((world-function system-library "llcore_stdin_port" (-> <port>)))))
    (define-r7rs current-output-port (make-parameter ((world-function system-library "llcore_stdout_port" (-> <port>)))))
    (define-r7rs current-error-port (make-parameter ((world-function system-library "llcore_stderr_port" (-> <port>)))))

    (define-r7rs eof-object? (make-predicate <eof-object>))
    (define-r7rs eof-object (native-function llbase "llbase_eof_object" (-> <eof-object>)))

    (define-syntax define-input-proc
      (syntax-rules (<native-int64>)
                    ((define-input-proc name native-symbol (-> <result-type>))
                     (define-r7rs name
                                  (let ((native-proc (world-function llbase native-symbol (-> <port> (U <result-type> <eof-object>)))))
                                    (lambda ([port : <port> (current-input-port)])
                                      (native-proc port)))))
                    ((define-input-proc name native-symbol (-> <native-int64> <result-type>))
                     (define-r7rs name
                                  (let ((native-proc (world-function llbase native-symbol (-> <native-int64> <port> (U <result-type> <eof-object>)))))
                                    (lambda ([count : <exact-integer>] [port : <port> (current-input-port)])
                                      (native-proc count port)))))))

    (define-input-proc read-u8 "llbase_read_u8" (-> <exact-integer>))
    (define-input-proc peek-u8 "llbase_peek_u8" (-> <exact-integer>))
    (define-input-proc read-char "llbase_read_char" (-> <char>))
    (define-input-proc peek-char "llbase_peek_char" (-> <char>))
    (define-input-proc read-line "llbase_read_line" (-> <string>))
    (define-input-proc read-bytevector "llbase_read_bytevector" (-> <native-int64> <bytevector>))
    (define-input-proc read-string "llbase_read_string" (-> <native-int64> <string>))

    (define native-read-bytevector! (world-function llbase "llbase_mutating_read_bytevector" (-> <bytevector> <port> <native-int64> <native-int64> (U <exact-integer> <eof-object>))))
    (define-r7rs (read-bytevector! [bv : <bytevector>] [port : <port> (current-input-port)] [start : <exact-integer> 0] [end : <exact-integer> (bytevector-length bv)])
                 (native-read-bytevector! bv port start end))

    (define native-u8-ready? (world-function llbase "llbase_u8_ready" (-> <port> <native-bool>)))
    (define-r7rs (u8-ready? [port : <port> (current-input-port)])
                 (native-u8-ready? port))

    (define native-char-ready? (world-function llbase "llbase_char_ready" (-> <port> <native-bool>)))
    (define-r7rs (char-ready? [port : <port> (current-input-port)])
                 (native-char-ready? port))

    (define native-newline (world-function llbase "llbase_newline" (-> <port> <unit>)))
    (define-r7rs (newline [port : <port> (current-output-port)])
                 (native-newline port))

    (define native-write-u8 (world-function llbase "llbase_write_u8" (-> <native-uint8> <port> <unit>)))
    (define-r7rs (write-u8 [byte : <exact-integer>] [port : <port> (current-output-port)])
                 (native-write-u8 byte port))

    (define native-write-char (world-function llbase "llbase_write_char" (-> <native-unicode-char> <port> <unit>)))
    (define-r7rs (write-char [char : <char>] [port : <port> (current-output-port)])
                 (native-write-char char port))

    (define native-write-string (world-function llbase "llbase_write_string" (-> <string> <port> <native-int64> <native-int64> <unit>)))
    (define-r7rs (write-string [str : <string>] [port : <port> (current-output-port)] [start : <exact-integer> 0] [end : <exact-integer> (string-length str)])
                 (native-write-string str port start end))

    (define native-write-bytevector (world-function llbase "llbase_write_bytevector" (-> <bytevector> <port> <native-int64> <native-int64> <unit>)))
    (define-r7rs (write-bytevector [str : <bytevector>] [port : <port> (current-output-port)] [start : <exact-integer> 0] [end : <exact-integer> (bytevector-length str)])
                 (native-write-bytevector str port start end))

    (define native-flush-output-port (world-function llbase "llbase_flush_output_port" (-> <port> <unit>)))
    (define-r7rs (flush-output-port [port : <port> (current-output-port)])
                 (native-flush-output-port port))

    (define-r7rs raise (world-function llbase "llbase_raise" (-> <any> <unit>) noreturn))
    (define-r7rs error (world-function llbase "llbase_error" (-> <string> <any> * <unit>) noreturn))
    (define-r7rs error-object? (make-predicate <error-object>))
    (define-r7rs error-object-message (native-function llbase "llbase_error_object_message" (-> <error-object> <string>)))
    (define-r7rs error-object-irritants (native-function llbase "llbase_error_object_irritants" (-> <error-object> <list>)))
    (define-r7rs file-error? (native-function llbase "llbase_is_file_error" (-> <any> <native-bool>)))
    (define-r7rs read-error? (native-function llbase "llbase_is_read_error" (-> <any> <native-bool>)))

    #| This is a native code helper which replaces most of the (guard) macro from R7RS with a much more efficient native
       native code implementation |#
    (define guard-kernel (world-function llbase "llbase_guard_kernel" (-> (-> <any> <any>) (-> <any>) <any>)))

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
)
