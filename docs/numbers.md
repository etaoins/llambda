Numbers
=======

Semantics
---------

Scheme defines an elaborate [numerical tower](http://en.wikipedia.org/wiki/Numerical_tower) ranging from integers to complex numbers. Llambda implements a strict subset of the numerical tower with two member types: ``<exact-integer>`` and ``<flonum>``. The builtin type of ``<number>`` can be used to refer any Scheme number.

``<exact-integer>`` is a 64-bit signed integer on all platforms. In Scheme terms they're considered exact numbers and can be introduced with constants such as ``15`` or ``#xdeadbeef``. If an operation that would normally produce another ``<exact-integer>`` encounters an integer overflow an ``integer-overflow-error`` will be signalled. Integer division by zero is prohibited and will signal an ``divide-by-zero-error``.

``<flonum>`` is an IEEE 64-bit double. This is the same representation JavaScript uses for its numbers with the same limitations on range and precision. In Scheme terms they're considered inexact numbers and can be introduced with constants such as ``4.5`` or ``9/2``. The special numbers ``+nan.0`` (Not A Number) ``+inf.0`` (positive infinity) and ``-inf.0`` (negative infinity) also have the type of ``<flonum>``. Division by a `<flonum>`` zero is permitted and results in an infinity.

Any arithmetic operation on mixed ``<exact-integer>`` and ``<flonum>`` operands will implicitly convert all of the operands to ``<flonum>`` and produce a ``<flonum>`` result. An ``<exact-integer>`` can also be explicitly converted in to a ``<flonum>`` using the ``(inexact)`` procedure. This can be useful for avoiding overflow when performing arithmetic on large integers at the expense of precision.

Complex numbers are completely unsupported. A ``(llambda complex-stub)`` library is supplied implementing the procedures defined in ``(scheme complex)``. However, they signal errors when creating or manipulating numbers with imaginary components. The library primarily exists as part of the R5RS compatibility implementation.

Performance
-----------

If a number's precise type is known it can be represented as a unboxed value and can have native code generated for its arithmetic operations. For this reason it's recommended that performance sensitive numeric code uses [type annotations](types.md) to communicate its expected types to the compiler. [Polymorphic procedures](types.md#polymorphic-procedures) can be used where the specific number types being used by a procedure aren't known ahead of time.

When performing integer division ``(truncate/)`` maps to LLVM's model of division while ``(floor/)`` and ``(modulo)`` are implemented in the runtime. If either procedure is suitable then using the ``(truncate/)`` family of procedures will result in significantly better performance.
