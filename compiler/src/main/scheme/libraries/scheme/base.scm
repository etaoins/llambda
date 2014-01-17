(define-library (scheme base)
	(import (rename (llambda internal primitives) (define-report-procedure define-r7rs)))
	(import (llambda nfi))

	; Export (lambda primitives) that occur in (scheme base)
	; These are virtual definitions provided by the compiler
	(export lambda quote if set! syntax-error include quasiquote unquote
			  unquote-splicing define define-syntax define-record-type
			  cond-expand)

	(export begin)
	(begin
	  (define-syntax begin
		 (syntax-rules ()
							((begin exp ...)
							 ((lambda () exp ...))))))

	(export let)
	(begin
	  ; This isn't the full definition - tagged let isn't supported
	  (define-syntax let
		 (syntax-rules ()
							((let ((name val) ...) body1 body2 ...)
							 ((lambda (name ...) body1 body2 ...)
							  val ...)))))

	(export and or when)
	(begin
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
								(begin result1 result2 ...))))))
	
	(export eqv? eq? equal?)
	(begin
	  (define-r7rs eqv? (native-function "lliby_is_eqv" (<datum-cell> <datum-cell>) <bool>))
	  (define-r7rs eq? eqv?)
	  (define-r7rs equal? (native-function "lliby_is_equal" (<datum-cell> <datum-cell>) <bool>)))

	(export number? real? rational? exact? exact-integer? inexact? exact inexact + - / *)
	(begin
	  (define-r7rs number? (native-function "lliby_is_numeric" (<datum-cell>) <bool>))
	  ; We only support real and rational numbers
	  (define-r7rs real? number?)
	  (define-r7rs rational? number?)
	  
	  (define-r7rs exact? (native-function "lliby_is_exact_integer" (<datum-cell>) <bool>))
	  (define-r7rs exact-integer? exact?)
	  (define-r7rs inexact? (native-function "lliby_is_inexact_rational" (<datum-cell>) <bool>))
	  
	  (define-r7rs exact (native-function "lliby_exact" (<numeric-cell>) <int64>))
	  (define-r7rs inexact (native-function "lliby_inexact" (<numeric-cell>) <double>))
	  
	  (define-r7rs + (native-function "lliby_add" <list-element-cell> <numeric-cell>))
	  (define-r7rs - (native-function "lliby_sub" (<numeric-cell> . <list-element-cell>) <numeric-cell>))
	  (define-r7rs * (native-function "lliby_mul" <list-element-cell> <numeric-cell>))
	  (define-r7rs / (native-function "lliby_div" (<numeric-cell> . <list-element-cell>) <double>)))

	(export boolean? not boolean=?)
	(begin
	  (define-r7rs boolean? (native-function "lliby_is_boolean" (<datum-cell>) <bool>))
	  (define-r7rs not (native-function "lliby_not" (<bool>) <bool>))
	  (define-r7rs boolean=? (native-function "lliby_boolean_equal" (<boolean-cell> <boolean-cell> . <list-element-cell>) <bool>)))

	(export pair? null? cons car cdr set-car! set-cdr! length list-copy
			make-list list append memv memq member)
	(begin 
	  (define-r7rs pair? (native-function "lliby_is_pair" (<datum-cell>) <bool>))
	  (define-r7rs null? (native-function "lliby_is_empty_list" (<datum-cell>) <bool>))
	  (define-r7rs cons (native-function "lliby_cons" (<datum-cell> <datum-cell>) <pair-cell>))
	  (define-r7rs car (native-function "lliby_car" (<pair-cell>) <datum-cell>))
	  (define-r7rs cdr (native-function "lliby_cdr" (<pair-cell>) <datum-cell>))
	  (define-r7rs set-car! (native-function "lliby_set_car" (<pair-cell> <datum-cell>)))
	  (define-r7rs set-cdr! (native-function "lliby_set_cdr" (<pair-cell> <datum-cell>)))
	  (define-r7rs length (native-function "lliby_length" (<list-element-cell>) <uint32>))
	  (define-r7rs list-copy (native-function "lliby_list_copy" (<datum-cell>) <datum-cell>))
	  (define-r7rs make-list (native-function "lliby_make_list" (<uint32> <datum-cell>) <list-element-cell>))
	  (define-r7rs list (native-function "lliby_list" <list-element-cell> <list-element-cell>))
	  (define-r7rs append (native-function "lliby_append" <list-element-cell> <datum-cell>))
	  (define-r7rs memv (native-function "lliby_memv" (<datum-cell> <list-element-cell>) <datum-cell>))
	  ; (eq?) is defined as (eqv?) so define (memq) as (memv)
	  (define-r7rs memq memv)
	  (define-r7rs member (native-function "lliby_member" (<datum-cell> <list-element-cell>) <datum-cell>)))

	(export symbol? symbol=? symbol->string string->symbol)
	(begin 
	  (define-r7rs symbol? (native-function "lliby_is_symbol" (<datum-cell>) <bool>))
	  (define-r7rs symbol=? (native-function "lliby_symbol_equal" (<symbol-cell> <symbol-cell> . <list-element-cell>) <bool>))
	  (define-r7rs symbol->string (native-function "lliby_symbol_to_string" (<symbol-cell>) <string-cell>))
	  (define-r7rs string->symbol (native-function "lliby_string_to_symbol" (<string-cell>) <symbol-cell>)))

	(export char? digit-value char->integer integer->char)
	(begin
	  (define-r7rs char? (native-function "lliby_is_character" (<datum-cell>) <bool>))
	  (define-r7rs digit-value (native-function "lliby_digit_value" (<unicode-char>) <datum-cell>))
	  (define-r7rs char->integer (native-function "lliby_char_to_integer" (<unicode-char>) <int32>))
	  (define-r7rs integer->char (native-function "lliby_integer_to_char" (<int32>) <unicode-char>)))
	
	(export vector? make-vector vector vector-length vector-ref vector-set! list->vector vector-append)
	(begin
	  (define-r7rs vector? (native-function "lliby_is_vector" (<datum-cell>) <bool>))
	  (define-r7rs make-vector (native-function "lliby_make_vector" (<uint32> <datum-cell>) <vector-cell>))
	  (define-r7rs vector (native-function "lliby_vector" <list-element-cell> <vector-cell>))
	  ; This is the same runtime function but instead of using a rest arg explicitly pass in the list
	  (define-r7rs list->vector (native-function "lliby_vector" (<list-element-cell>) <vector-cell>))
	  (define-r7rs vector-length (native-function "lliby_vector_length" (<vector-cell>) <uint32>))
	  (define-r7rs vector-ref (native-function "lliby_vector_ref" (<vector-cell> <uint32>) <datum-cell>))
	  (define-r7rs vector-set! (native-function "lliby_vector_set" (<vector-cell> <uint32> <datum-cell>)))
	  (define-r7rs vector-append (native-function "lliby_vector_append" <list-element-cell> <vector-cell>)))
	
	(export bytevector? make-bytevector bytevector bytevector-length bytevector-u8-ref bytevector-u8-set! bytevector-append)
	(begin
	  (define-r7rs bytevector? (native-function "lliby_is_bytevector" (<datum-cell>) <bool>))
	  (define-r7rs make-bytevector (native-function "lliby_make_bytevector" (<uint32> <uint8>) <bytevector-cell>))
	  (define-r7rs bytevector (native-function "lliby_bytevector" <list-element-cell> <bytevector-cell>))
	  (define-r7rs bytevector-length (native-function "lliby_bytevector_length" (<bytevector-cell>) <uint32>))
	  (define-r7rs bytevector-u8-ref (native-function "lliby_bytevector_u8_ref" (<bytevector-cell> <uint32>) <uint8>))
	  (define-r7rs bytevector-u8-set! (native-function "lliby_bytevector_u8_set" (<bytevector-cell> <uint32> <uint8>)))
	  (define-r7rs bytevector-append (native-function "lliby_bytevector_append" <list-element-cell> <bytevector-cell>)))
	
	(export string? make-string string string-length string-ref string-set! string-append list->string)
	(begin
	  (define-r7rs string? (native-function "lliby_is_string" (<datum-cell>) <bool>))
	  (define-r7rs make-string (native-function "lliby_make_string" (<uint32> <unicode-char>) <string-cell>))
	  (define-r7rs string (native-function "lliby_string" <list-element-cell> <string-cell>))
	  ; This is the same runtime function but instead of using a rest arg explicitly pass in the list
	  (define-r7rs list->string (native-function "lliby_string" (<list-element-cell>) <string-cell>))
	  (define-r7rs string-length (native-function "lliby_string_length" (<string-cell>) <uint32>))
	  (define-r7rs string-ref (native-function "lliby_string_ref" (<string-cell> <uint32>) <unicode-char>))
	  (define-r7rs string-set! (native-function "lliby_string_set" (<string-cell> <uint32> <unicode-char>)))
	  (define-r7rs string-append (native-function "lliby_string_append" <list-element-cell> <string-cell>)))

	(export procedure? apply)
	(begin
	  (define-r7rs procedure? (native-function "lliby_is_procedure" (<datum-cell>) <bool>))
	  (define-r7rs apply (native-function "lliby_apply" (<procedure-cell> . <list-element-cell>) <datum-cell>)))

	(export features)
	(begin
	  ; Note this is produced by codegen; it's not part of the standard library
	  (define-r7rs features (native-function "__llambda_features" () <list-element-cell>)))
)
