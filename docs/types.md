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
| ``<pair>``          | Standard Scheme pair
| ``<empty-list>``    | The empty list value. This is also known as "null" in some Lisp languages
| ``<list-element>``  | Union of ``<pair>`` and ``<empty-list>``
| ``<string>``        | Scheme string
| ``<symbol>``        | Scheme symbol. Specific symbols can be used as by quoting them, for example ``'one`` is the type of the "one" symbol
| ``<exact-integer>`` | 64-bit signed integer. This is used to represent lengths and indices as well as being suitable for direct arithmetic
| ``<flonum>``        | 64-bit IEEE floating point value
| ``<number>``        | Union of ``<exact-integer>`` and ``<flonum>``
| ``<char>``          | Scheme character. All valid Unicode characters are supported
| ``<vector>``        | Scheme vector
| ``<bytevector>``    | Scheme bytevector
| ``<procedure>``     | General procedure type. More specific procedure types are available through the ``->`` type constructor
| ``<port>``          | Scheme port
| ``<eof-object>``    | End-of-file object returned by ``(read)`` procedure
| ``<unit>``          | Unit type, also known as ``void`` in some languages. This is used by procedures not returning a value

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

For example, a procedure taking an integer and a <string and returning a character has the following type
```racket
(-> <exact-integer> <string> <char>)
```

If a procedure takes a rest argument list a ``*`` can be suffixed after the final argument. For example, a prodcure taking zero or more floating point numbers and returning a boolean has the following type
```racket
(-> <flonum> * <exact-integer>)
```

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

Annotating Procedures
---------------------
If the complete type for a procedure is known it is often the simplest and most idiomatic to use a forward type declaration with a procedure type
```racket
(: multiply-by-two (-> <number> <number>))
(define (multiply-by-two val)
  (* val 2))
```

However, it is also possible to annotate the individual arguments to a procedure using ``[arg : <type>]`` syntax.
```racket
(define (multiply-by-two [val : <number>])
  (* val 2))
```

The square brackets are an extension to R7RS that are treated identically to round brackets. They are only used in type annotations by convention. This identical to the behaviour and conventions of Typed Racket.

Procedures taking a rest argument list need to use ``rest : <type> *``. A procedure taking another procedure and zero or more integers would look like the following
```racket
(define (sum-integers [inexact-result : <boolean>] ints : <exact-integer> *)
  (define sum (apply + ints))
  (if inexact-result (inexact sum) sum))
