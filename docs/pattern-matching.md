Pattern Matching
================

Llambda provide a simple [pattern matching](http://en.wikipedia.org/wiki/Pattern_matching) facility in the ``(llambda match)`` library. Pattern matching allows specific data structures to be matched and can extract values from within those structures. This is particularly useful for actors to recognise and deconstruct incoming messages.

The ``(match)`` primitive is the basis of pattern matching. It takes a single value to match followed by one or more clauses. The clauses are evaluated in order until a match is encountered. When a clause matches the body of the clause is evaluated and its value is returned as the value of the ``(match)`` expression. If none of the clauses match then a ``match-error`` will be signalled.

Literal Matches
---------------
Any self-evaluating value (such as numbers, strings, bytevectors, etc) or quoted data will match that literal value if it compares as ``(equal?)``. These aren't of much utility by themselves but can be used to build more complex patterns to match partially constant data.

The following code will convert the exact integers 1-3 to descriptive symbols. Any other value will signal a ``match-error``
```racket
(match input-val
  [1 'one]
  [2 'two]
  [3 'three])
```

Any Value Matches
-----------------
An unquoted symbol will match any value and bind that value in the clause's body. This can be used to extract dynamic values from complex data structures. The special ``_`` symbol matches any value without binding the value; this lets it function as a wildcard.

The following code will convert the strings "true" and "false" to their boolean counterparts and takes the boolean value of any other value.
```racket
(match input-val
  ["true" #t]
  ["false" #f]
  [other (if other #t #f)])
```

Intrinsic Constructor Matches
-----------------------------
The constructors ``(cons)``, ``(list)`` and ``(vector)`` match pairs, lists and vectors with the provided member patterns. ``(cons)`` matches any pair with the given ``car`` and ``cdr`` subpatterns. ``(list)`` matches any proper list where the member subpatterns match each member in the list. ``(vector)`` works similarly except with vectors.

The following code will match lists or vectors with two elements and return the elements in a pair.
```racket
(match two-element-value
  [(list first second) (cons first second)]
  [(vector first second) (cons first second)])

```

Record Type Constructor Matches
-------------------------------
User defined record type constructors can also be used in pattern matching. The member patterns in the constructor match the corresponding field they initialise. Fields that don't appear in the constructor cannot be pattern matched.

Actors can use user defined record types to send and receive messages in a type safe manner. Each message the actor can receive can be defined as a record type and the body of the actor's behaviour can match incoming messages against the message types it expects.

The following code will match a ``<point>`` record type and determine if the ``x`` or ``y`` value is zero.
```racket
(define-record-type <point> (point x y) point?
  [[x : <number>] point-x]
  [[y : <number>] point-y])

(match point-val
  [(point 0 _) 'zero-x]
  [(point _ 0) 'zero-y]
  [(point _ _) 'nonzero])
```

Typed Value Matches
-------------------
Type annotation syntax can be used to match values of a specific type. This requires importing ``(llambda typed)`` for the ``:`` symbol.

The following code will return string description of some basic values.
```racket
(match value
  [#f
    "Boolean true"]
  [#t
    "Boolean false"]
  [[_ : <string>]
    "String"]
  [[_ : <symbol>]
    "Symbol"]
  [+nan.0
    "NaN"]
  [[_ : <exact-integer>]
    "Integer"]
  [[_ : <flonum>]
    "Floating point number"])

```

``(match-lambda)``
------------------
``(match-lambda)`` is a macro for creating a lambda taking a single argument and then immediately matching that argument. The body of the ``(match-lambda)`` consists of a series of clauses to match the argument against.

The following code defines a recursive lambda to sum the values in a provided list

```racket
(define sum-list (match-lambda
  ['() 0]
  [(cons head tail) (+ head (sum-list tail))]))
```
