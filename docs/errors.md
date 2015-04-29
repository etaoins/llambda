Errors
======

Error Model
-----------

Llambda implements the R7RS error system which is built on exceptions raised by ``(raise)`` or ``(error)`` and caught with ``(guard)`` or ``(with-exception-handler)``. Individual error categories have predicate procedures that can be used to categorise signalled errors inside exception handlers. If an exception is not caught by an exception handler a description of the error will be printed to standard error and the program will terminate.

In this example ``(open-input-file)`` is executed and a message is printed whenever a ``file-error`` is signalled. If any other error is signalled the exception will be re-raised and handled by the next exception handler.

```racket
(guard (condition
         ; Catch file errors and let other errors propagate
         ((file-error? condition)
          (write "Unable to open file!")
          (newline)
          (exit -1)))
       ; Try to open the path under (guard)
       (open-input-file "/some/path"))
```

If the Llambda compiler determines that code will always cause an error to be signalled it will produce an error message and abort compilation. For example, ``(/ 1 0)`` or ``(abs 'notanumber)`` would both be rejected as invalid by the compiler. This differs from some Scheme implementations that will only signal errors once the offending code is executed.

Extended Error Categories
-------------------------

R7RS only defines two error categories:

1. ``file-error`` is signalled whenever an operation on a file fails. This includes missing files and permission errors.
2. ``read-error`` is signalled when the ``(read)`` procedure is unable to parse its input.

Llambda extends this error system with a number of additional error categories. Procedures for these extended categories are available in the ``(llambda error)`` library. The predicate procedure for an error category is named ``(category-name?)`` while the raise procedure is called ``(raise-category-name)``. For example, the ``divide-by-zero-error`` predicate is ``(divide-by-zero-error?)`` and its raise procedure is ``(raise-divide-by-zero-error)``.

| Error Category                       | Signalling Condition
|--------------------------------------|---------------------
| ``arity-error``                      | Procedure called with an incompatible number of arguments
| ``divide-by-zero-error``             | Integer division by zero was attempted. See the [numbers documentation](numbers.md) for more information.
| ``implementation-restriction-error`` | Llambda-specific limitation was encountered in an otherwise valid program
| ``integer-overflow-error``           | Integer overflow encountered during arithmetic. See the [numbers documentation](numbers.md) for more information.
| ``invalid-argument-error``           | Invalid argument supplied to a procedure where a more specific error was not applicable
| ``mutate-literal-error``             | Attempted mutation of a constant string, pair, vector or bytevector
| ``out-of-memory-error``              | Process memory exhausted. This is only raised in certain situations; memory exhaustion is usually fatal.
| ``range-error``                      | Collection was indexed outside the bounds of the collection
| ``type-error``                       | Runtime type check failed
| ``undefined-variable-error``         | Recursive variable referenced before its definition
| ``utf8-error``                       | Invalid UTF-8 encoding was encountered
| ``match-error``                      | Pattern matching failed to match any clauses
