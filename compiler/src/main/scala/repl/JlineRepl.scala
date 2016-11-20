package io.llambda.compiler.repl
import io.llambda

import io.llambda.compiler._

import java.io.File
import scala.collection.JavaConverters._
import jline.console.{ConsoleReader, history, completer}
import annotation.tailrec

/** Tab completion based on the current scope */
private class ScopeCompleter(scope : Scope) extends completer.Completer {
  private def identifierStart(buffer : String, start : Int, end : Int) : Int = {
    if (start > 0) {
      val newStart = start - 1
      val testIdent = buffer.drop(newStart).take(end - newStart)

      if (SchemeParser.isValidIdentifier(testIdent)) {
        // There's more identifier to find
        identifierStart(buffer,newStart, end)
      }
      else {
        // Ran out of identifier
        start
      }
    }
    else {
      // We reached the beginning
      0
    }
  }

  def complete(buffer : String, cursor : Int, candidates : java.util.List[CharSequence]) : Int = {
    if (buffer == null) {
      candidates.addAll(scope.bindings.keys.toList.asJava)
      0
    }
    else {
      val identStart = identifierStart(buffer, cursor, cursor)
      val identEnd = cursor

      val ident = buffer.drop(identStart).take(identEnd - identStart)
      val rawCompletions = scope.bindings.keys.filter(_.startsWith(ident)).toList

      val completions = rawCompletions.toList match {
        case List(single) =>
          // This is unambiguous - add a space after to tell the user we're done
          List(single + " ")

        case other =>
          other.sorted
      }

      candidates.addAll(completions.asJava)
      identStart
    }
  }
}

class JlineRepl(targetPlatform : platform.TargetPlatform) {
  // ANSI colour codes
  private val failureColour = 31
  private val successColour = 32

  // Create our REPL evaluator
  var evaluator = new Evaluator(targetPlatform)

  // Create our reader
  val reader = new ConsoleReader;

  // Make our history dir
  val llambdaDir = new File(System.getProperty("user.home"), ".llambda")
  llambdaDir.mkdir()

  // Set our history file
  val historyFile = new File(llambdaDir, "repl-history")
  reader.setHistory(new history.FileHistory(historyFile))

  // Update our initial auto-completion set
  setCompleter(new ScopeCompleter(evaluator.scope))

  // Don't expand events - ! is used frequently in Scheme identifiers
  reader.setExpandEvents(false)

  private def setCompleter(newCompleter : completer.Completer) : Unit = {
    for(completer <- reader.getCompleters.asScala) {
      reader.removeCompleter(completer)
    }

    reader.addCompleter(newCompleter)
  }

  private def colourStr(str : String, colourCode : Int) =
    s"\u001B[${colourCode}m${str}\u001B[0m"

  def evaluate(userString : String) {
    try {
      val data = SchemeParser.parseStringAsData(userString, Some("input"))

      for(datum <- data) {
        println(colourStr("res: ", successColour) + evaluator(datum))
      }
    }
    catch {
      case semantic : SemanticException =>
        val colouredError = colourStr(semantic.semanticErrorType, failureColour)
        println(s"${colouredError}: ${semantic.getMessage}")

      case parse : ParseErrorException =>
        println(colourStr("parse error: ", failureColour) + parse.getMessage)

      case nonzero : ReplProcessNonZeroExitException  =>
        if (!nonzero.stdout.isEmpty) {
          println(s"${nonzero.stdout}")
        }

        if (!nonzero.stderr.isEmpty) {
          println(s"${nonzero.stderr}")
        }

        println(colourStr("non-zero exit: ", failureColour) + nonzero.getMessage)
    }

    println()
  }

  @tailrec
  private def acceptInput()(implicit reader : ConsoleReader) {
    val command = reader.readLine("llambda> ")

    // Flush the last command to our history file
    reader.getHistory match {
      case flushable : history.PersistentHistory =>
        flushable.flush()
    }

    command match {
      case ":reset" =>
        // Rebuild our evaluator
        evaluator = new Evaluator(targetPlatform)

        setCompleter(new ScopeCompleter(evaluator.scope))
        acceptInput()

      case ":quit" =>
        // Don't recurse

      case ":help" =>
        println("Special commands:")
        println(":reset\t\tResets the REPL state including definitions and imported libraries")
        println(":quit \t\tQuits the REPL")
        println(":help \t\tPrints this help")
        println()
        println("All other input is treated as a top-level Scheme expression")

        acceptInput()

      case userString =>
        evaluate(userString)
        acceptInput()
    }
  }

  def apply() {
    try {
      acceptInput()(reader)
    }
    finally {
      reader.getTerminal.restore()
    }
  }
}
