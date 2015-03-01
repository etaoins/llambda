Scheme Support
==============

R7RS Base Language
------------------

The core Llambda language is based closely on the [Scheme R7RS report](http://trac.sacrideo.us/wg/raw-attachment/wiki/WikiStart/r7rs.pdf) except for the intentional caveats described in the [dialects documentation](dialects.md). However, the R7RS implementation is incomplete and contains a number of unintentional shortcomings:

* Circular lists are not supported. In the default ``llambda`` dialect it's impossible to create circular lists due to pairs being immutable. In the ``r7rs`` and ``r5rs`` dialects passing circular lists to procedures expecting lists will cause an infinite loop.
* The use of datum labels to create lists with shared or circular structures is not supported. Datum labels are only usable after their definition
* The parser case folding directives ``#!fold-case`` and ``#!no-fold-case`` are not supported
* Case insensitive parsing via ``(include-ci)`` does not parse character names case insensitively. Symbol names are lowercased instead of case folded which causes incorrect behaviour with certain non-ASCII characters
* ``(define-record-type)`` must use fields with distinct names even if their identifiers are in different scopes. This is used to implement record inheritance but may cause problems with record types defined in macros

The [numbers documentation](numbers.md) describes the subset of the R7RS numerical tower implemented by Llambda.


R7RS Optional Library Support
-----------------------------

R7RS defines a number of additional libraries that implementations may provide. Llambda implements the majority of them with the exception of ``(scheme complex)`` and libraries related to runtime evaluation.

| Library Name                 | Description                                         | Support
|------------------------------|-----------------------------------------------------|--------
| ``(scheme case-lambda)``     | Exports ``(case-lambda)``                           | Complete
| ``(scheme char)``            | Procedures for dealing with characters              | Complete
| ``(scheme complex)``         | Procedures useful for non-real numbers              | None; stub in ``(llambda complex-stub)``
| ``(scheme cxr)``             | Compositions of ``(car)`` and ``(cdr)``             | Complete
| ``(scheme eval)``            | Support for runtime evaluation                      | None
| ``(scheme file)``            | Support for accessing files                         | Complete
| ``(scheme inexact)``         | Procedures for dealing with inexact numbers         | Complete
| ``(scheme lazy)``            | Support for lazy evaluation                         | Complete
| ``(scheme load)``            | Support for loading Scheme source code              | None
| ``(scheme process-context)`` | Support for accessing a program's calling context   | Complete
| ``(scheme read)``            | Support for reading Scheme data                     | Complete
| ``(scheme repl)``            | Exports ``(interaction-environment)``               | None
| ``(scheme time)``            | Access to time-related values                       | Complete
| ``(scheme write)``           | Support for writing Scheme data                     | Partial; datum labels are unsupported
| ``(scheme r5rs)``            | R5RS compatibility                                  | Partial; no runtime evaluation support

SRFI-1
------

Llambda includes a partial implementation of the [SRFI-1](http://srfi.schemers.org/srfi-1/srfi-1.html) list library as ``(llambda list)``. All procedures depending on mutable pairs or already exported by ``(scheme base)`` are explicitly excluded. Additionally, many of the more obscure procedures have yet to be implemented.

``(llambda list)`` exports the following SRFI-1 compatible procedures: ``(cons*)``, ``(xcons)``, ``(list-tabulate)``, ``(iota)``, ``(fold)``, ``(reduce)``, ``(zip)``, ``(filter)``, ``(remove)``, ``(find)``, ``(find-tail)``, ``(partition)``, ``(take)``, ``(drop)``, ``(split-at)``, ``(take-while)``, ``(drop-while)``, ``(span)``, ``(break)``, ``(any)``, ``(every)``, ``(count)``, ``(append-map)``, ``(filter-map)``.
