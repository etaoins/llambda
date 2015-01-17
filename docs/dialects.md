Dialects
========

Llambda supports three different Scheme dialects selectable using the ``--scheme-dialect`` option to the compiler. The entire program must be compiled with the same dialect. If no dialect is specified then the ``llambda`` dialect is used by default.

All dialects are variations on a core shared language implementation. The [Scheme support documentation](scheme.md) for information on the Scheme language implemented by Llambda.

``llambda``
-----------

The ``llambda`` dialect is extremely similar to the standard R7RS language with two important differences:

1. All pairs are immutable; ``(set-car!)``, ``(set-cdr!)`` and ``(list-set!)`` are not defined. This is used to implement efficient types for pairs and lists. See the [types documentation](types.md#value-type-constructors) for more information.
2. Multiple top-level ``(defines)`` of the same identifier are not allowed. If an identifier needs to be bound to a new value then ``(set!)`` should be used explicitly.

``r7rs``
--------

The ``r7rs`` dialect follows standard R7RS as closely as possible. It is identical to the ``llambda`` dialect but with mutable pairs and top-level redefinitions allowed.

``r5rs``
--------

The ``r5rs`` dialect is a compatibility mode for working with legacy R5RS programs. This causes the root program file to be parsed case insensitively and the ``(scheme r5rs)`` library to be implicitly imported. This is an imperfect emulation of R5RS; see the "Incompatibilities with R5RS" section of the [R7RS report](http://trac.sacrideo.us/wg/raw-attachment/wiki/WikiStart/r7rs.pdf) for more information.

Because type constructors typically contain uppercase letters they are inaccessible from ``r5rs`` programs. This means that type annotation support in ``r5rs`` programs is very restricted. Developers wanting more comprehensive type support should use the ``llambda`` or ``r7rs`` dialects.
