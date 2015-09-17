Introduction
============

Llambda is a natively compiled Scheme with optional strong typing. The core language is based on [R7RS](http://trac.sacrideo.us/wg/raw-attachment/wiki/WikiStart/r7rs.pdf) with a number of extensions influenced by [Typed Racket](http://docs.racket-lang.org/ts-guide/) and the [SRFI](http://srfi.schemers.org) community.

Llambda implements many of the features expected from a modern programming language including:

* First class functions
* Unicode support including UTF-8 strings
* Support for functional programming including higher-order functions such as ``map``, ``reduce`` and ``fold``
* Safe programming environment with garbage collected memory, enforced bounds checking and checked integer overflow
* Concurrency support via an implementation of the [Actor model](http://en.wikipedia.org/wiki/Actor_model)
* Pattern matching
* Read-evaluate-print loop

Llambda is implemented with a [Scala](http://www.scala-lang.org) frontend, [LLVM](http://llvm.org) backend and a Scheme and C++11 runtime.

The language is currently very experimental with all non-R7RS language features in flux. Although it's well tested through an extensive functional test suite very few non-trivial programs have been written in Llambda. The lack of bindings for any non-system libraries make it only suitable for standalone programs operating on standard I/O and files.

Requirements
============

* Modern Unix-like operating system such as Mac OS X, Linux or FreeBSD. Llambda is explicitly tested on Mac OS X 10.10, Ubuntu 15.04 and FreeBSD 10.1 after every major change.
* [CMake](http://www.cmake.org) 2.8
* [LLVM](http://llvm.org) 3.6
* [Clang](http://clang.llvm.org) 3.6
* [sbt](http://www.scala-sbt.org)

Installation
============

Ensure that the above requirements are met and the related programs are in your user's path. Then, the runtime can be built using CMake.

```
$ cd llambda
$ mkdir build
$ cd build
$ cmake ../runtime && make
```

Usage
=====

Llambda takes Scheme source files as input and produces standalone statically linked executables. A trivial "Hello, World" application would look like:
```racket
(import (scheme base))
(import (scheme write))

(write "Hello, world!")
(newline)
```

The ``llambda`` shell script be can used used to compile Scheme programs by passing their path on the command line. It supports various options which are described by ``llambda --help``. Of particular interest is the ``-s`` option which will immediately run the program instead of producing an exectuable. This can be useful for scripting tasks.

For frequent compilation it is more efficient to use ``sbt`` to run the compiler from within the ``sbt`` environment. This avoids the overhead of launching a JVM for every invokation of the compiler. For example, to compile a program located at ``/home/example/test.scm`` would work as follows:

```
$ cd llambda/
$ sbt
> project compiler
> run /home/ryan/Code/test/test.scm
```

Documentation
=============

Generated documentation is hosted at [llambda.readthedocs.org](http://llambda.readthedocs.org/)
