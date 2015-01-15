REPL
====

Running Llambda without any arguments will launch a simple [REPL](http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop). This allows simple Scheme expressions to be evaluated and their results printed to the console. Typing ``:quit`` will exit the REPL and ``:reset`` will reset all definitions and imported libraries.

Simple line editing, history and tab completion are provided by [jline2](https://github.com/jline/jline2). This does not work well from inside ``sbt`` as already configures the terminal for line editing. For best results the REPL should be invoked using the ``llambda`` shell script with no arguments.

As Llambda is a compiled-only language the REPL actually creates and runs a temporary program for each expression. The REPL will attempt to detect new definitions and persist them when evaluating future expressions. This is imperfect and doesn't work correctly in many complex use cases. If misbehaviour is encountered it may be better to create a scratch Scheme file and use ``llambda -s`` to run that file.
