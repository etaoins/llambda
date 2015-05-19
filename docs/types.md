Type System
===========

Llambda's type system is inspired by [Typed Racket](http://docs.racket-lang.org/ts-guide/) but is less sophisticated. Unlike Typed Racket typing is completely optional; untyped R7RS Scheme programs can be gradually annotated with types to check for errors, improve performance or as self-documentation.

Llambda automatically extracts type information from programs without explicit type annotations. Checking a value's type using type predicates or applying a procedure that expects certain types will be used to dynamically build a model of the types in a program. This model is then used to check for incorrect code and perform optimisations.

Explicit type annotation support is available from the ``(llambda typed)`` library. The examples in this document assume it has been imported.

Core Types
----------
The core types are provided by the compiler. By convention all type names are enclosed in angle brackets.

| Type Name           | Description
|---------------------|------------
| ``<any>``           | Any Scheme type. This is the top type which contains all other types
| ``<boolean>``       | ``#t`` or ``#f``. The literal ``#t`` or ``#f`` can also be used as type names to represent a specific boolean value.
| ``<bytevector>``    | Scheme bytevector
| ``<char>``          | Scheme character. All valid Unicode characters are supported
| ``<empty-list>``    | The empty list value. This is also known as "null" in some Lisp languages
| ``<eof-object>``    | End-of-file object returned by ``(read)`` procedure
| ``<exact-integer>`` | 64-bit signed integer. This is used to represent lengths and indices as well as being suitable for direct arithmetic
| ``<flonum>``        | 64-bit IEEE floating point value
| ``<list-element>``  | Union of ``<pair>`` and ``<empty-list>``
| ``<number>``        | Union of ``<exact-integer>`` and ``<flonum>``
| ``<pair>``          | Standard Scheme pair
| ``<port>``          | Scheme port
| ``<procedure>``     | General procedure type. More specific procedure types are available through the ``->`` type constructor
| ``<string>``        | Scheme string
| ``<symbol>``        | Scheme symbol. Specific symbols can be used as by quoting them, for example ``'one`` is the type of the "one" symbol
| ``<unit>``          | Unit type, also known as ``void`` in some languages. This is used by procedures not returning a value
| ``<vector>``        | Scheme vector

More information on the numeric types can be found in the [numbers documentation](numbers.md).

Value Type Constructors
-----------------------
Type constructors work in a similar fashion to type constructors in Typed Racket. They can be considered analogous to normal procedures but instead of accepting values and returning values they accept type arguments and return a new type. For example, the ``Pairof`` type constructor can be applied as ``(Pairof <boolean> <char>)``. This constructs a pair type where the ``car`` is a ``<boolean>`` and the ``cdr`` is a ``<char>``

By convention type constructors are named with an initial uppercase letter

| Type Constructor          | Description
|---------------------------|------------
| ``(U <member> ...)``      | Creates a union of the passed types. For example, the ``<number>`` type is equivalent to ``(U <exact-integer> <flonum>)``. |
| ``(Pairof <car> <cdr>)``  | Creates a pair type with the passed ``car`` and ``cdr`` types
| ``(Listof <member>)``     | Creates a proper list type with the containing members of type ``<member>``. The proper list can be of any length.
| ``(List <member> ...)``   | Creates a proper list type of fixed length with the specified member types

The ``Pairof``, ``Listof`` and ``List`` type constructors depend on immutable pair support for a complete and efficient implementation. If mutable pairs are enabled by using the [``r7rs`` or ``r5rs`` dialects](dialects.md) these type constructors can only be used for arguments and return types. They will also generate costly runtime type checks in more situations than dialects with immutable pairs.

Procedure Type Constructor
--------------------------
The procedure type constructor is ``->``. It defines which values a procedure takes and which values it returns. The non-terminal arguments to the type constructor are the types of the arguments the procedure takes and the final argument is the value it returns.

For example, a procedure taking an integer and a string and returning a character has the following type
```racket
(-> <exact-integer> <string> <char>)
```

If a procedure takes a rest argument list a ``*`` can be suffixed after the final argument. For example, a procedure taking zero or more floating point numbers and returning a boolean has the following type
```racket
(-> <flonum> * <exact-integer>)
```

Scheme procedures can return multiple values using the ``(values)`` procedure. To indicate a procedure returns a specific number values the ``(Values)`` type constructor can be used with the types of the values returned. Alternatively, the special ``*`` type can be used to indicate the procedure returns an arbitrary number of values with unknown types.

Annotating Values
-----------------
There are two ways to annotate values. The way most familiar to developers from other languages would be explicitly passing the type to ``(define)``. For example, to define ``b`` to have the value ``15`` and the type ``<number>`` we would use the following form
```racket
(define b : <number> 15)
```

For immutable values these annotations are primarily useful as documentation since the compiler can statically determine the type of the value in most situations. However, with mutable values they can provide an important hint to both the developer and the compiler about which values the variable is intended to take. For example, the following would be caught as an error by the compiler
```racket
(define b : <number> 15)
(set! b "Not a number") ; Not allowed
```

Another way to annotate values that's common in the Lisp languages is by providing a forward type declaration. The above define is exactly equivalent to
```racket
(: b <number>)
(define b 15)
```

The forward annotation can appear anywhere in the same scope before the matching definition. If an annotation doesn't match an eventual definition an error will be raised. This can be useful for separating type annotations from code or when using complex types such as procedure types.

Annotating Expressions
----------------------
Expressions can be annotated as producing a value of a given type. `(ann)` ensures that an expression has a given type at compile time. If it's possible for the expression to produce an incompatible type the compilation will fail.
```racket
(ann (+ 2 5) <number>) ;; => 7
```

Alternatively `(cast)` can be used to create a runtime check for a type. If the expression produces an incompatible type then a `type-error` will be signalled.
```racket
(cast (string->number "56") <exact-integer>) ;; => 56
```

Annotating Procedures
---------------------
If the complete type for a procedure is known it is often the simplest and most idiomatic to use a forward type declaration with a procedure type
```racket
(: multiply-by-two (-> <number> <number>))
(define (multiply-by-two val)
  (* val 2))

(multiply-by-two 4) ;; => 8
```

However, it is also possible to annotate the individual arguments to a procedure using ``[arg : <type>]`` syntax.
```racket
(define (multiply-by-two [val : <number>])
  (* val 2))

(multiply-by-two -2.5) ;; => -5.0
```

The square brackets are an extension to R7RS that are treated identically to round brackets. They are only used in type annotations by convention. This identical to the behaviour and conventions of Typed Racket.

Procedures taking a rest argument list need to use ``rest : <type> *``. A procedure taking another procedure and zero or more integers would look like the following
```racket
(define (sum-integers [inexact-result : <boolean>] ints : <exact-integer> *)
  (define sum (apply + ints))
  (if inexact-result (inexact sum) sum))

(sum-integers #t 1 2 3 4 5) ;; => 15.0
```

Polymorphic Procedures
----------------------

Polymorphic procedures have "type variables" inside their signature that are resolved to specific types each time the procedure is used. This allows the procedure to work with multiple types of data while still benefitting from type safety.

This is formally known as [parametric polymorphism](http://en.wikipedia.org/wiki/Parametric_polymorphism) and is similar to generics in Java to templates in C++. As in C++ the Llambda compiler will generate separate specialised native functions for each input type.

There are two main reasons to use polymorphic procedures over Scheme's normal dynamic typing:

* A procedure may have a return type that depends on its input types and it wants to communicate that return type to its callers for type safety or performance. This is particularly common with procedures dealing with lists or numbers.
* For performance reasons a specialised version of the procedure should be constructed for each input type. This is especially useful with procedures working with numbers or procedure values as it can allow much more efficient code to be generated.

Currently polymorphic procedures can only be created by a type declaration using the ``(All)`` type constructor. For example, a procedure to reverse a list could be defined as follows:

```racket
; This is an awful implementation of (reverse) in (scheme base)
(: rev (All (A) (Listof A) (Listof A)))
(define (rev l)
  (if (null? l)
    l
    (append (rev (cdr l)) (list (car l)))))

(rev '(1 2 3 4 5)) ;; => '(5 4 3 2 1)
```

This declares that ``rev`` takes a list and returns a list of the same type. If this isn't true an error will be signaled. Typically violations are caught at compile time but some situations will result in runtime checks being generated.

Type variables can also have upper bounds placed on them. This declares that the type variable must resolve to the upper bound or one of its subtypes

For example, the following procedure will multiply any ``<number>`` value by 2

```racket
(: times-2 (All ([N : <number>]) N N))
(define (times-2 v)
  (* 2 v))

(times-2 5.0) ;; => 10.0
```

Multiple type variables can appear in a type declaration. The following procedure swaps the car and cdr of a pair while preserving its type information

```racket
(: swap-pair (All (A D) (Pairof A D) (Pairof D A)))
(define (swap-pair p)
  (cons (cdr p) (car p)))

(swap-pair '(1 . 2)) ;; => '(2 . 1)
```

Record Types
------------

Llambda extends the record types in R7RS to support various typing features. This allows them to be used similarly to algebraic data types in other functional languages, especially when combined with [pattern matching](pattern-matching.md).

Every record type definition in Llambda introduces a distinct first class type bound to the first argument to ``(define-record-type)``. This can be used to refer to the record type anywhere a type is expected, such as type annotations.

```racket
(define-record-type <point> (point x y) point?
  [x point-x]
  [y point-y])

(: add-points (-> <point> <point> <point>))
(define (add-points first second)
  (point
    (+ (point-x first) (point-x second))
    (+ (point-y first) (point-y second))))

(add-points (point 10 20) (point 30 40)) ; => (point 40 60)
```

Typed fields allow record type definitions to specify any field's type using type annotation syntax on the field's name. This can have benefits for type safety, self-documentation and performance. As the default field initialiser is ``<unit>`` fields with types not including ``<unit>`` must be explicitly initialised by appearing in the constructor.


```racket
(define-record-type <file-position> (file-position filename line column) file-position?
  [[filename : <string>] file-position-filename]
  [[line : <exact-integer>] file-position-line]
  [[column : <exact-intger>] file-position-column])
```

Record type inheritance allows a record type to inherit the fields of another. A child record type instance can be used whenever an instance of the parent type is expected. Inheritance is specified providing a parent record type name as an additional argument after the child record type's name. Fields in any inherited record types can be initialised by specifying their field name in the child record type's constructor.

```racket
(define-record-type <point> (point x y) point?
  [[x : <flonum>] point-x]
  [[y : <flonum>] point-y])

(define-record-type <point-3d> <point> (point-3d x y z) point-3d?
  [[z : <flonum>] point-z])

(point? (point-3d 1.0 2.0 3.0)) ;; => #t
```
