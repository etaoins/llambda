(define-library (scheme base)
	(import (llambda primitives))
	(import (llambda nfi))
	(import (rename (llambda internal) (define-report-procedure define-r7rs)))

	; Export (lambda primitives)
	; These are virtual definitions provided by the compiler
	(export lambda quote if set! syntax-error include quasiquote unquote unquote-splicing)

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

	(export number? real? rational? exact? exact-integer? inexact? exact inexact + - / *)
	(begin
	  (define-r7rs number? (native-function "lliby_is_numeric" (boxed-datum) bool))
	  ; We only support real and rational numbers
	  (define-r7rs real? number?)
	  (define-r7rs rational? number?)
	  
	  (define-r7rs exact? (native-function "lliby_is_exact_integer" (boxed-datum) bool))
	  (define-r7rs exact-integer? exact?)
	  (define-r7rs inexact? (native-function "lliby_is_inexact_rational" (boxed-datum) bool))
	  
	  (define-r7rs exact (native-function "lliby_exact" (boxed-numeric) int64))
	  (define-r7rs inexact (native-function "lliby_inexact" (boxed-numeric) double))
	  
	  (define-r7rs + (native-function "lliby_add" boxed-list-element boxed-numeric))
	  (define-r7rs - (native-function "lliby_sub" (boxed-numeric . boxed-list-element) boxed-numeric))
	  (define-r7rs * (native-function "lliby_mul" boxed-list-element boxed-numeric))
	  (define-r7rs / (native-function "lliby_div" (boxed-numeric . boxed-list-element) double)))

	(export boolean? not boolean=?)
	(begin
	  (define-r7rs boolean? (native-function "lliby_is_boolean" (boxed-datum) bool))
	  (define-r7rs not (native-function "lliby_not" (truthy-bool) bool))
	  (define-r7rs boolean=? (native-function "lliby_boolean_equal" (strict-bool strict-bool . boxed-list-element) bool)))

	(export pair? null? cons car cdr set-car! set-cdr! length list-copy make-list list)
	(begin 
	  (define-r7rs pair? (native-function "lliby_is_pair" (boxed-datum) bool))
	  (define-r7rs null? (native-function "lliby_is_empty_list" (boxed-datum) bool))
	  (define-r7rs cons (native-function "lliby_cons" (boxed-datum boxed-datum) boxed-pair))
	  (define-r7rs car (native-function "lliby_car" (boxed-pair) boxed-datum))
	  (define-r7rs cdr (native-function "lliby_cdr" (boxed-pair) boxed-datum))
	  (define-r7rs set-car! (native-function "lliby_set_car" (boxed-pair boxed-datum) void))
	  (define-r7rs set-cdr! (native-function "lliby_set_cdr" (boxed-pair boxed-datum) void))
	  (define-r7rs length (native-function "lliby_length" (boxed-list-element) uint32))
	  (define-r7rs list-copy (native-function "lliby_list_copy" (boxed-list-element) boxed-list-element))
	  (define-r7rs make-list (native-function "lliby_make_list" (uint32 boxed-datum) boxed-list-element))
	  (define-r7rs list (native-function "lliby_list" boxed-list-element boxed-list-element)))

	(export char? digit-value char->integer integer->char)
	(begin
	  (define-r7rs char? (native-function "lliby_is_character" (boxed-datum) bool))
	  (define-r7rs digit-value (native-function "lliby_digit_value" (unicode-char) boxed-datum))
	  (define-r7rs char->integer (native-function "lliby_char_to_integer" (unicode-char) int32))
	  (define-r7rs integer->char (native-function "lliby_integer_to_char" (int32) unicode-char)))
	
	(export vector? make-vector vector vector-length vector-ref vector-set! list->vector)
	(begin
	  (define-r7rs vector? (native-function "lliby_is_vector" (boxed-datum) bool))
	  (define-r7rs make-vector (native-function "lliby_make_vector" (uint32 boxed-datum) boxed-vector))
	  (define-r7rs vector (native-function "lliby_vector" boxed-list-element boxed-vector))
	  ; This is the same runtime function but instead of using a rest arg explicitly pass in the list
	  (define-r7rs list->vector (native-function "lliby_vector" (boxed-list-element) boxed-vector))
	  (define-r7rs vector-length (native-function "lliby_vector_length" (boxed-vector) uint32))
	  (define-r7rs vector-ref (native-function "lliby_vector_ref" (boxed-vector uint32) boxed-vector))
	  (define-r7rs vector-set! (native-function "lliby_vector_set" (boxed-vector uint32 boxed-datum) void)))
	
	(export string? make-string string string-length string-ref string-set! string-append list->string)
	(begin
	  (define-r7rs string? (native-function "lliby_is_string" (boxed-datum) bool))
	  (define-r7rs make-string (native-function "lliby_make_string" (uint32 unicode-char) boxed-string))
	  (define-r7rs string (native-function "lliby_string" boxed-list-element boxed-string))
	  ; This is the same runtime function but instead of using a rest arg explicitly pass in the list
	  (define-r7rs list->string (native-function "lliby_string" (boxed-list-element) boxed-string))
	  (define-r7rs string-length (native-function "lliby_string_length" (boxed-string) uint32))
	  (define-r7rs string-ref (native-function "lliby_string_ref" (boxed-string uint32) unicode-char))
	  (define-r7rs string-set! (native-function "lliby_string_set" (boxed-string uint32 unicode-char) void))
	  (define-r7rs string-append (native-function "lliby_string_append" boxed-list-element boxed-string)))
)
