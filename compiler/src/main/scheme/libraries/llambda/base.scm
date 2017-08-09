(define-library (llambda base)
  (import (llambda nfi))
  (import (rename (llambda internal primitives) (define-stdlib-procedure define-stdlib)))
  (import (llambda internal features))
  (import (llambda error))

  ; export (llambda internal primitives) that occur in (llambda base)
  ; these are virtual definitions provided by the compiler
  (export lambda quote if else set! syntax-error include quasiquote unquote unquote-splicing define define-syntax
          syntax-rules define-record-type cond-expand parameterize)

  (export begin)
  (export let let* letrec* letrec let-syntax letrec-syntax)
  (export cond case and or when unless)
  (export do)
  (export _ ...)
  (export =>)
  (export eqv? equal?)
  (export number? integer? flonum? rational? zero? even? odd? integer flonum + - / * expt = < > <= >= positive?
          negative? floor ceiling truncate round square abs truncate/ truncate-quotient truncate-remainder floor/
          floor-quotient floor-remainder max min gcd lcm integer-sqrt)
  (export number->string string->number)
  (export boolean? not boolean=?)
  (export pair? null? list? cons car cdr caar cadr cdar cddr length make-list list append memv member assv assoc reverse
          list-tail list-ref)

  (export symbol? symbol=? symbol->string string->symbol)
  (export char? char->integer integer->char char=? char<? char>? char<=? char>=?)
  (export vector? make-vector vector vector-length vector-ref vector-set! list->vector vector->list vector-append
          vector-copy vector-copy! vector-fill! string->vector vector->string)
  (export bytevector? make-bytevector bytevector bytevector-length bytevector-u8-ref bytevector-u8-set!
          bytevector-append utf8->string string->utf8 bytevector-copy bytevector-copy!)
  (export string? make-string string string-length string-ref string-append list->string string->list string-copy
          substring string=? string<? string>? string<=? string>=?)
  (export procedure? apply vector-map vector-for-each map for-each string-map string-for-each)
  (export make-parameter)
  (export port? input-port? output-port? current-input-port current-output-port current-error-port input-port-open?
          output-port-open? close-port close-input-port close-output-port open-output-string get-output-string
          open-output-bytevector get-output-bytevector open-input-string open-input-bytevector call-with-port)
  (export eof-object? eof-object read-u8 peek-u8 read-char peek-char read-line read-bytevector read-string
          read-bytevector! u8-ready? char-ready?)
  (export newline write-u8 write-char write-string write-bytevector flush-output-port)
  (export features)
  (export raise error error-object? error-object-message error-object-irritants guard file-error? read-error?)

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
                                #!unit
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
         (define-stdlib (name [source : <source-type>] [start : <integer> 0] [end : <integer> (source-length-proc source)])
           (native-proc source start end)))))

    (define-syntax define-mutating-copy-proc
      (syntax-rules ()
        ((define-mutating-copy-proc name native-proc <type> length-proc)
         (define-stdlib (name [to : <type>] [at : <integer>] [from : <type>] [start : <integer> 0] [end : <integer> (length-proc from)])
           (native-proc to at from start end)))))

    (define-native-library llbase (static-library "ll_llambda_base"))

    ; Define the length accessors for slicing values
    (define-stdlib vector-length (native-function llbase "llbase_vector_length" (-> <vector> <native-int64>) nocapture))
    (define-stdlib bytevector-length (native-function llbase "llbase_bytevector_length" (-> <bytevector> <native-int64>) nocapture))
    (define-stdlib string-length (native-function llbase "llbase_string_length" (-> <string> <native-uint32>) nocapture))

    (define-stdlib eqv? (native-function system-library "llcore_is_eqv" (-> <any> <any> <native-bool>) nocapture))
    (define-stdlib equal? (native-function system-library "llcore_is_equal" (-> <any> <any> <native-bool>) nocapture))

    (define-stdlib boolean? (make-predicate <boolean>))
    (define-stdlib not (make-predicate #f))
    (define-stdlib boolean=? (native-function llbase "llbase_boolean_equal" (-> <boolean> <boolean> <boolean> * <native-bool>) nocapture))

    (define-stdlib procedure? (make-predicate <procedure>))
    (define-stdlib apply (world-function llbase "llbase_apply" (-> <procedure> <any> * <any>)))

    (define-stdlib number? (make-predicate <number>))
    (define-stdlib flonum? (make-predicate <flonum>))
    (define-stdlib integer? (make-predicate <integer>))

    (define (rational? [val : <any>])
      (cond
        ((integer? val) #t)
        ((flonum? val)
         ; XXX: This would be more idiomatic as a (memv) but our optimiser has issues getting rid of the temporary list
         (not (or (eqv? val +nan.0) (eqv? val +inf.0) (eqv? val -inf.0))))
        (else #f)))

    (define-stdlib = (native-function llbase "llbase_numeric_equal" (-> <number> <number> <number> * <native-bool>) nocapture))
    (define-stdlib < (native-function llbase "llbase_numeric_lt" (-> <number> <number> <number> * <native-bool>) nocapture))
    (define-stdlib > (native-function llbase "llbase_numeric_gt" (-> <number> <number> <number> * <native-bool>) nocapture))
    (define-stdlib <= (native-function llbase "llbase_numeric_lte" (-> <number> <number> <number> * <native-bool>) nocapture))
    (define-stdlib >= (native-function llbase "llbase_numeric_gte" (-> <number> <number> <number> * <native-bool>) nocapture))

    ; These branch on type as our planner currently won't optimise comparisons without a definite type
    (define-stdlib (zero? [n : <number>])
      (if (integer? n) (= n 0) (= n 0.0)))

    (define-stdlib (positive? [n : <number>])
      (if (integer? n) (> n 0) (> n 0.0)))

    (define-stdlib (negative? [n : <number>])
      (if (integer? n) (< n 0) (< n 0.0)))

    (define native-floor (native-function system-library "floor" (-> <native-double> <native-double>)))
    (: floor (All ([N : <number>]) (-> N N)))
    (define-stdlib (floor n)
      (if (integer? n) n (native-floor n)))

    (define native-ceil (native-function system-library "ceil" (-> <native-double> <native-double>)))
    (: ceiling (All ([N : <number>]) (-> N N)))
    (define-stdlib (ceiling n)
      (if (integer? n) n (native-ceil n)))

    (define native-trunc (native-function system-library "trunc" (-> <native-double> <native-double>)))
    (: truncate (All ([N : <number>]) (-> N N)))
    (define-stdlib (truncate n)
      (if (integer? n) n (native-trunc n)))

    (define native-round (native-function system-library "round" (-> <native-double> <native-double>)))
    (: round (All ([N : <number>]) (-> N N)))
    (define-stdlib (round n)
      (if (integer? n) n (native-round n)))

    (define-stdlib integer (world-function llbase "llbase_integer" (-> <number> <native-int64>)))
    (define-stdlib flonum (native-function llbase "llbase_flonum" (-> <number> <native-double>) nocapture))

    (define-stdlib + (world-function llbase "llbase_add" (All ([N : <number>]) N * N) nocapture))
    (define-stdlib - (world-function llbase "llbase_sub" (All ([N : <number>]) N N * N) nocapture))
    (define-stdlib * (world-function llbase "llbase_mul" (All ([N : <number>]) N * N) nocapture))
    (define-stdlib / (world-function llbase "llbase_div" (-> <number> <number> * <number>)))

    (define-stdlib expt (world-function llbase "llbase_expt" (All ([N : <number>]) (-> N N N)) nocapture))

    (: square (All ([N : <number>]) (-> N N)))
    (define-stdlib (square num)
      (* num num))

    (: abs (All ([N : <number>]) (-> N N)))
    (define-stdlib (abs num)
      ; Do a top-level type check to make the compiler generate a specialised version of each branch. The test itself is
      ; semantically a no-op
      (if (integer? num)
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

    (define-stdlib truncate/ (world-function llbase "llbase_truncate_div" (-> <native-int64> <native-int64> (Pairof <integer> <integer>))))
    (define-stdlib truncate-quotient (world-function llbase "llbase_truncate_quotient" (-> <native-int64> <native-int64> <native-int64>)))
    (define-stdlib truncate-remainder (world-function llbase "llbase_truncate_remainder" (-> <native-int64> <native-int64> <native-int64>)))

    (define-stdlib floor/ (world-function llbase "llbase_floor_div" (-> <native-int64> <native-int64> (Pairof <integer> <integer>))))
    (define-stdlib floor-quotient (world-function llbase "llbase_floor_quotient" (-> <native-int64> <native-int64> <native-int64>)))
    (define-stdlib floor-remainder (world-function llbase "llbase_floor_remainder" (-> <native-int64> <native-int64> <native-int64>)))

    (define-stdlib (odd? [val : <integer>])
                 (not (= (truncate-remainder val 2) 0)))

    (define-stdlib (even? [val : <integer>])
                 (= (truncate-remainder val 2) 0))

    (define-stdlib max (native-function llbase "llbase_max" (All ([N : <number>]) N N * N)))
    (define-stdlib min (native-function llbase "llbase_min" (All ([N : <number>]) N N * N)))

    (define native-gcd (native-function llbase "llbase_gcd" (-> <native-int64> <native-int64> <integer> * <native-int64>) nocapture))
    (define-stdlib gcd (case-lambda
                       (() 0)
                       (([single : <integer>]) (abs single))
                       (rest (apply native-gcd rest))))

    (define native-lcm (native-function llbase "llbase_lcm" (-> <native-int64> <native-int64> <integer> * <native-int64>) nocapture))
    (define-stdlib lcm (case-lambda
                       (() 1)
                       (([single : <integer>]) (abs single))
                       (rest (apply native-lcm rest))))

    (define-stdlib integer-sqrt (world-function llbase "llbase_integer_sqrt" (-> <native-int64> (Pairof <integer> <integer>))))

    (define native-number->string (world-function llbase "llbase_number_to_string" (-> <number> <native-uint8> <string>)))
    (define-stdlib (number->string [num : <number>] [radix : <integer> 10])
      (native-number->string num radix))

    (define native-string->number (world-function llbase "llbase_string_to_number" (-> <string> <native-uint8> (U #f <number>))))
    (define-stdlib (string->number [str : <string>] [radix : <integer> 10])
      (native-string->number str radix))

    (define-stdlib pair? (make-predicate <pair>))
    (define-stdlib null? (make-predicate <empty-list>))
    (define-stdlib list? (make-predicate <list>))

    (define-stdlib length (native-function llbase "llbase_length" (-> <list> <native-uint32>) nocapture))

    (define-stdlib cons (world-function llbase "llbase_cons" (All (A B) A B (Pairof A B))))
    (define-stdlib car (native-function llbase "llbase_car" (All (A) (Pairof A <any>) A)))
    (define-stdlib cdr (native-function llbase "llbase_cdr" (All (A) (Pairof <any> A) A)))
    (define-stdlib (caar (x : (Pairof <pair> <any>))) (car (car x)))
    (define-stdlib (cadr (x : (Pairof <any> <pair>))) (car (cdr x)))
    (define-stdlib (cdar (x : (Pairof <pair> <any>))) (cdr (car x)))
    (define-stdlib (cddr (x : (Pairof <any> <pair>))) (cdr (cdr x)))

    (: list (All (A) (-> A * (Listof A))))
    (define-stdlib (list . rest) rest)

    (define-stdlib append (world-function llbase "llbase_append" (-> <any> * <any>)))

    (define-stdlib memv (native-function llbase "llbase_memv" (All (A) A (Listof A) (U (Listof A) #f))))
    (define-stdlib member (native-function llbase "llbase_member" (All (A) A (Listof A) (U (Listof A) #f))))

    (define-stdlib assv (native-function llbase "llbase_assv" (All (A B) A (Alistof A B) (U (Pairof A B) #f))))
    (define-stdlib assoc (native-function llbase "llbase_assoc" (All (A B) A (Alistof A B) (U (Pairof A B) #f))))

    (define-stdlib reverse (world-function llbase "llbase_reverse" (All (A) (Listof A) (Listof A))))

    (define-stdlib list-tail (world-function llbase "llbase_list_tail" (All (A) (Listof A) <native-uint32> (Listof A))))

    (: list-ref (All (A) (-> (Listof A) <integer> A)))
    (define-stdlib (list-ref l n)
      (define tail (list-tail l n))
      (if (pair? tail)
        (car tail)
        (raise-range-error "(list-ref) at exact end of list" n)))

    (define native-make-list (world-function llbase "llbase_make_list" (All (A) <native-uint32> A (Listof A))))
    (define-stdlib (make-list [len : <integer>] [fill : <any> #!unit])
      (native-make-list len fill))

    (define-stdlib symbol? (make-predicate <symbol>))
    (define-stdlib symbol=? (native-function llbase "llbase_symbol_equal" (-> <symbol> <symbol> <symbol> * <native-bool>) nocapture))
    (define-stdlib symbol->string (world-function llbase "llbase_symbol_to_string" (-> <symbol> <string>)))
    (define-stdlib string->symbol (world-function llbase "llbase_string_to_symbol" (-> <string> <symbol>)))

    (define-stdlib char? (make-predicate <char>))
    (define-stdlib char->integer (native-function llbase "llbase_char_to_integer" (-> <native-unicode-char> <native-int32>)))
    (define-stdlib integer->char (world-function llbase "llbase_integer_to_char" (-> <native-int64> <native-unicode-char>)))
    (define-stdlib char=? (native-function llbase "llbase_char_equal" (-> <native-unicode-char> <native-unicode-char> <char> * <native-bool>) nocapture))
    (define-stdlib char<? (native-function llbase "llbase_char_lt" (-> <native-unicode-char> <native-unicode-char> <char> * <native-bool>) nocapture))
    (define-stdlib char>? (native-function llbase "llbase_char_gt" (-> <native-unicode-char> <native-unicode-char> <char> * <native-bool>) nocapture))
    (define-stdlib char<=? (native-function llbase "llbase_char_lte" (-> <native-unicode-char> <native-unicode-char> <char> * <native-bool>) nocapture))
    (define-stdlib char>=? (native-function llbase "llbase_char_gte" (-> <native-unicode-char> <native-unicode-char> <char> * <native-bool>) nocapture))

    (define-stdlib vector? (make-predicate <vector>))
    (define-stdlib vector (world-function llbase "llbase_vector" (-> <any> * <vector>)))
    ; This is the same runtime function but instead of using a rest arg explicitly pass in the list
    (define-stdlib list->vector (world-function llbase "llbase_vector" (-> <list> <vector>)))
    (define-stdlib vector-ref (world-function llbase "llbase_vector_ref" (-> <vector> <native-int64> <any>)))
    (define-stdlib vector-set! (world-function llbase "llbase_vector_set" (-> <vector> <native-int64> <any> <unit>)))
    (define-stdlib vector-append (world-function llbase "llbase_vector_append" (-> <vector> * <vector>)))

    (define native-vector->list (world-function llbase "llbase_vector_to_list" (-> <vector> <native-int64> <native-int64> <list>)))
    (define-slice-proc vector->list native-vector->list <vector> vector-length)

    (define native-vector-copy (world-function llbase "llbase_vector_copy" (-> <vector> <native-int64> <native-int64> <vector>)))
    (define-slice-proc vector-copy native-vector-copy <vector> vector-length)

    (define native-vector-copy! (world-function llbase "llbase_vector_mutating_copy" (-> <vector> <native-int64> <vector> <native-int64> <native-int64> <unit>)))
    (define-mutating-copy-proc vector-copy! native-vector-copy! <vector> vector-length)

    (define native-vector-fill! (world-function llbase "llbase_vector_mutating_fill" (-> <vector> <any> <native-int64> <native-int64> <unit>)))
    (define-stdlib (vector-fill! [target : <vector>] [fill : <any>] [start : <integer> 0] [end : <integer> (vector-length target)])
      (native-vector-fill! target fill start end))

    (define native-make-vector (world-function llbase "llbase_make_vector" (-> <native-int64> <any> <vector>)))
    (define-stdlib (make-vector [len : <integer>] [fill : <any> #!unit])
      (native-make-vector len fill))

    (define native-vector->string (world-function llbase "llbase_vector_to_string" (-> <vector> <native-int64> <native-int64> <string>)))
    (define-slice-proc vector->string native-vector->string <vector> vector-length)

    (define native-string->vector (world-function llbase "llbase_string_to_vector" (-> <string> <native-int64> <native-int64> <vector>)))
    (define-slice-proc string->vector native-string->vector <string> string-length)

    (define-stdlib bytevector? (make-predicate <bytevector>))
    (define-stdlib bytevector (world-function llbase "llbase_bytevector" (-> <integer> * <bytevector>)))
    (define-stdlib bytevector-u8-ref (world-function llbase "llbase_bytevector_u8_ref" (-> <bytevector> <native-int64> <native-uint8>)))
    (define-stdlib bytevector-u8-set! (world-function llbase "llbase_bytevector_u8_set" (-> <bytevector> <native-int64> <native-uint8> <unit>)))
    (define-stdlib bytevector-append (world-function llbase "llbase_bytevector_append" (-> <bytevector> * <bytevector>)))

    (define native-bytevector-copy (world-function llbase "llbase_bytevector_copy" (-> <bytevector> <native-int64> <native-int64> <bytevector>)))
    (define-slice-proc bytevector-copy native-bytevector-copy <bytevector> bytevector-length)

    (define native-bytevector-copy! (world-function llbase "llbase_bytevector_mutating_copy" (-> <bytevector> <native-int64> <bytevector> <native-int64> <native-int64> <unit>)))
    (define-mutating-copy-proc bytevector-copy! native-bytevector-copy! <bytevector> bytevector-length)

    (define native-utf8->string (world-function llbase "llbase_utf8_to_string" (-> <bytevector> <native-int64> <native-int64> <string>)))
    (define-slice-proc utf8->string native-utf8->string <bytevector> bytevector-length)

    (define native-make-bytevector (world-function llbase "llcore_bytevector_alloc_filled" (-> <native-int64> <native-uint8> <bytevector>)))
    (define-stdlib (make-bytevector [len : <integer>] [fill : <integer> 0])
      (native-make-bytevector len fill))

    (define-stdlib string? (make-predicate <string>))
    (define-stdlib make-string (world-function llbase "llbase_make_string" (-> <native-int64> <native-unicode-char> <string>)))
    (define-stdlib string (world-function llbase "llbase_string" (-> <char> * <string>) nocapture))
    ; This is the same runtime function but instead of using a rest arg explicitly pass in the list
    (define-stdlib list->string (world-function llbase "llbase_string" (-> (Listof <char>) <string>) nocapture))
    (define-stdlib string-ref (world-function llbase "llbase_string_ref" (-> <string> <native-int64> <native-unicode-char>)))
    (define-stdlib string-append (world-function llbase "llbase_string_append" (-> <string> * <string>)))

    (define native-string->list (world-function llbase "llbase_string_to_list" (-> <string> <native-int64> <native-int64> (Listof <char>))))
    (define-slice-proc string->list native-string->list <string> string-length)

    (define native-string->utf8 (world-function llbase "llbase_string_to_utf8" (-> <string> <native-int64> <native-int64> <bytevector>)))
    (define-slice-proc string->utf8 native-string->utf8 <string> string-length)

    #| Unlike other slicing functions the raw slicer is exposed as (substring) to implement the procedure with the same
       name defined in R7RS |#
    (define-stdlib substring (world-function llbase "llbase_substring" (-> <string> <native-int64> <native-int64> <string>)))
    (define-slice-proc string-copy substring <string> string-length)

    (define-stdlib string=? (native-function llbase "llbase_string_equal" (-> <string> <string> <string> * <native-bool>) nocapture))
    (define-stdlib string<? (native-function llbase "llbase_string_lt" (-> <string> <string> <string> * <native-bool>) nocapture))
    (define-stdlib string>? (native-function llbase "llbase_string_gt" (-> <string> <string> <string> * <native-bool>) nocapture))
    (define-stdlib string<=? (native-function llbase "llbase_string_lte" (-> <string> <string> <string> * <native-bool>) nocapture))
    (define-stdlib string>=? (native-function llbase "llbase_string_gte" (-> <string> <string> <string> * <native-bool>) nocapture))

    (define-stdlib vector-map (world-function llbase "llbase_vector_map" (-> (-> <any> <any> * <any>) <vector> <vector> * <vector>)))
    (define-stdlib vector-for-each (world-function llbase "llbase_vector_for_each" (-> (-> <any> <any> * <unit>) <vector> <vector> * <unit>)))

    (define-stdlib map (world-function llbase "llbase_map" (All (A) (-> <any> <any> * A) <list> <list> * (Listof A))))
    (define-stdlib for-each (world-function llbase "llbase_for_each" (-> (-> <any> <any> * <unit>) <list> <list> * <unit>)))

    (define-stdlib string-map (world-function llbase "llbase_string_map" (-> (-> <char> <char> * <char>) <string> <string> * <string>)))
    (define-stdlib string-for-each (world-function llbase "llbase_string_for_each" (-> (-> <char> <char> * <unit>) <string> <string> * <unit>)))

    (define-stdlib make-parameter (world-function system-library "llcore_make_parameter" (-> <any> <procedure>)))

    ; Port support
    (define-stdlib port? (make-predicate <port>))
    (define-stdlib input-port? (native-function llbase "llbase_is_input_port" (-> <any> <native-bool>) nocapture))
    (define-stdlib output-port? (native-function llbase "llbase_is_output_port" (-> <any> <native-bool>) nocapture))
    (define-stdlib input-port-open? (native-function llbase "llbase_is_input_port_open" (-> <port> <native-bool>) nocapture))
    (define-stdlib output-port-open? (native-function llbase "llbase_is_output_port_open" (-> <port> <native-bool>) nocapture))
    (define-stdlib close-port (native-function llbase "llbase_close_port" (-> <port> <unit>) nocapture))
    (define-stdlib close-input-port (world-function llbase "llbase_close_input_port" (-> <port> <unit>)))
    (define-stdlib close-output-port (world-function llbase "llbase_close_output_port" (-> <port> <unit>)))
    (define-stdlib open-output-string (world-function llbase "llbase_open_output_string" (-> <port>)))
    (define-stdlib get-output-string (world-function llbase "llbase_get_output_string" (-> <port> <string>)))
    (define-stdlib open-output-bytevector (world-function llbase "llbase_open_output_bytevector" (-> <port>)))
    (define-stdlib get-output-bytevector (world-function llbase "llbase_get_output_bytevector" (-> <port> <bytevector>)))
    (define-stdlib open-input-string (world-function llbase "llbase_open_input_string" (-> <string> <port>)))
    (define-stdlib open-input-bytevector (world-function llbase "llbase_open_input_bytevector" (-> <bytevector> <port>)))
    (define-stdlib call-with-port (world-function llbase "llbase_call_with_port" (-> <port> (-> <port> <any>) <any>)))

    (define-stdlib current-input-port (make-parameter ((native-function system-library "llcore_stdin_port" (-> <port>)))))
    (define-stdlib current-output-port (make-parameter ((native-function system-library "llcore_stdout_port" (-> <port>)))))
    (define-stdlib current-error-port (make-parameter ((native-function system-library "llcore_stderr_port" (-> <port>)))))

    (define-stdlib eof-object? (make-predicate <eof-object>))
    (define-stdlib eof-object (native-function llbase "llbase_eof_object" (-> <eof-object>)))

    (define-syntax define-input-proc
      (syntax-rules (<native-int64>)
                    ((define-input-proc name native-symbol (-> <result-type>))
                     (define-stdlib name
                                  (let ((native-proc (world-function llbase native-symbol (-> <port> (U <result-type> <eof-object>)))))
                                    (lambda ([port : <port> (current-input-port)])
                                      (native-proc port)))))
                    ((define-input-proc name native-symbol (-> <native-int64> <result-type>))
                     (define-stdlib name
                                  (let ((native-proc (world-function llbase native-symbol (-> <native-int64> <port> (U <result-type> <eof-object>)))))
                                    (lambda ([count : <integer>] [port : <port> (current-input-port)])
                                      (native-proc count port)))))))

    (define-input-proc read-u8 "llbase_read_u8" (-> <integer>))
    (define-input-proc peek-u8 "llbase_peek_u8" (-> <integer>))
    (define-input-proc read-char "llbase_read_char" (-> <char>))
    (define-input-proc peek-char "llbase_peek_char" (-> <char>))
    (define-input-proc read-line "llbase_read_line" (-> <string>))
    (define-input-proc read-bytevector "llbase_read_bytevector" (-> <native-int64> <bytevector>))
    (define-input-proc read-string "llbase_read_string" (-> <native-int64> <string>))

    (define native-read-bytevector! (world-function llbase "llbase_mutating_read_bytevector" (-> <bytevector> <port> <native-int64> <native-int64> (U <integer> <eof-object>))))
    (define-stdlib (read-bytevector! [bv : <bytevector>] [port : <port> (current-input-port)] [start : <integer> 0] [end : <integer> (bytevector-length bv)])
                 (native-read-bytevector! bv port start end))

    (define native-u8-ready? (world-function llbase "llbase_u8_ready" (-> <port> <native-bool>)))
    (define-stdlib (u8-ready? [port : <port> (current-input-port)])
                 (native-u8-ready? port))

    (define native-char-ready? (world-function llbase "llbase_char_ready" (-> <port> <native-bool>)))
    (define-stdlib (char-ready? [port : <port> (current-input-port)])
                 (native-char-ready? port))

    (define native-newline (world-function llbase "llbase_newline" (-> <port> <unit>)))
    (define-stdlib (newline [port : <port> (current-output-port)])
                 (native-newline port))

    (define native-write-u8 (world-function llbase "llbase_write_u8" (-> <native-uint8> <port> <unit>)))
    (define-stdlib (write-u8 [byte : <integer>] [port : <port> (current-output-port)])
                 (native-write-u8 byte port))

    (define native-write-char (world-function llbase "llbase_write_char" (-> <native-unicode-char> <port> <unit>)))
    (define-stdlib (write-char [char : <char>] [port : <port> (current-output-port)])
                 (native-write-char char port))

    (define native-write-string (world-function llbase "llbase_write_string" (-> <string> <port> <native-int64> <native-int64> <unit>)))
    (define-stdlib (write-string [str : <string>] [port : <port> (current-output-port)] [start : <integer> 0] [end : <integer> (string-length str)])
                 (native-write-string str port start end))

    (define native-write-bytevector (world-function llbase "llbase_write_bytevector" (-> <bytevector> <port> <native-int64> <native-int64> <unit>)))
    (define-stdlib (write-bytevector [str : <bytevector>] [port : <port> (current-output-port)] [start : <integer> 0] [end : <integer> (bytevector-length str)])
                 (native-write-bytevector str port start end))

    (define native-flush-output-port (world-function llbase "llbase_flush_output_port" (-> <port> <unit>)))
    (define-stdlib (flush-output-port [port : <port> (current-output-port)])
                 (native-flush-output-port port))

    (define-stdlib raise (world-function llbase "llbase_raise" (-> <any> <unit>) noreturn))
    (define-stdlib error (world-function llbase "llbase_error" (-> <string> <any> * <unit>) noreturn))
    (define-stdlib error-object? (make-predicate <error-object>))
    (define-stdlib error-object-message (native-function llbase "llbase_error_object_message" (-> <error-object> <string>)))
    (define-stdlib error-object-irritants (native-function llbase "llbase_error_object_irritants" (-> <error-object> <list>)))
    (define-stdlib file-error? (native-function llbase "llbase_is_file_error" (-> <any> <native-bool>) nocapture))
    (define-stdlib read-error? (native-function llbase "llbase_is_read_error" (-> <any> <native-bool>) nocapture))

    #| This is a native code helper which replaces most of the (guard) macro from R7RS with a much more efficient native
       code implementation |#
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
