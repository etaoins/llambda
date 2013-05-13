package llambda

import scala.tools.jline.console.ConsoleReader
import annotation.tailrec

/** Base class for all REPL modes */
abstract class ReplMode(val name : String) {
  def evaluate(input : String) : Unit
}

/** Base class for REPL modes that involve parsing scheme */
abstract class SchemeParsingMode(name : String) extends ReplMode(name) {
  def evalDatum(data : ast.Datum) : String

  def evaluate(userString : String) {
    SchemeParser(userString) match {
      case SchemeParser.Success(data, _) => 
        try {
          for(datum <- data) {
            println("res: " + evalDatum(datum))
          }
        }
        catch {
          case malformed : MalformedExpressionException =>
            println("malformed: " + malformed.getMessage)
          case badspecial : BadSpecialFormException =>
            println("bad special form: " + badspecial.getMessage)
          case unbound : UnboundVariableException =>
            println("unbound variable: " + unbound.getMessage)
          case libnotfound : LibraryNotFoundException =>
            println("library not found: " + libnotfound.getMessage)
        }
      case err =>
        println("error: " + err)
    }
  }
}

/** Just parses Scheme */
class ParseOnlyMode extends SchemeParsingMode("parse") {
  def evalDatum(datum : ast.Datum) : String =
    datum.toString
}

/** Extract expressions allowed in a library, program or lambda body */
class BodyExpressionMode extends SchemeParsingMode("body") {
  private val schemeCoreBindings = (new DefaultLibraryLoader).loadSchemeCore
  implicit var currentScope = new Scope(collection.mutable.Map(schemeCoreBindings.toSeq : _*))

  def evalDatum(datum : ast.Datum) = {
    datum match {
      case ast.ProperList(ast.Symbol("import") :: _) =>
        // This is an import decl - import our new bindings
        val loader = new DefaultLibraryLoader
        val newBindings = ResolveImportDecl(datum)(loader.load)

        currentScope = currentScope ++ newBindings

        "loaded"
      case _ =>
        // Treat this like a body expression
        val (exprs, newScope) = ExtractBody(datum :: Nil)(currentScope)

        currentScope = newScope
        exprs.map(_.toString).mkString(" ")
    }
  }
}

/** Implements the REPL loop and switching modes */
object Repl {
  @tailrec
  private def acceptInput(mode : ReplMode)(implicit reader : ConsoleReader) {
    val command = reader.readLine(mode.name + "> ")

    command match {
      case ":quit" =>
        return;

      case ":parse" =>
        acceptInput(new ParseOnlyMode)
      
      case ":body" =>
        acceptInput(new BodyExpressionMode)

      case userString =>
        mode.evaluate(userString)
        acceptInput(mode)
    }
  }

  def apply() {
    val reader = new ConsoleReader;
    acceptInput(new BodyExpressionMode)(reader)
  }
}
