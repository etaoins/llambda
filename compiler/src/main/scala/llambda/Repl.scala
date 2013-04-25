package llambda

import scala.tools.jline.console.ConsoleReader
import annotation.tailrec

/** Base class for all REPL modes */
abstract class ReplMode(val name : String) {
  def evaluate(input : String) : Unit
}

/** Base class for REPL modes that involve parsing scheme */
abstract class SchemeParsingMode(name : String) extends ReplMode(name) {
  def evalData(data : List[ast.Datum]) : Unit

  def evaluate(userString : String) {
    SchemeParser(userString) match {
      case SchemeParser.Success(expressions, _) => 
        evalData(expressions)
      case err =>
        println("error: " + err)
    }
  }
}

/** Just parses Scheme */
class ParseOnlyMode extends SchemeParsingMode("parse") {
  def evalData(data : List[ast.Datum]) {
    for(datum <- data) {
      println("res: " + datum)
    }
  }
}

/** Extracts primitive expressions from (scheme core) */
class PrimitiveExpressionMode extends SchemeParsingMode("primitive") {
  implicit val primitiveScope = new Scope(SchemePrimitives.bindings)

  def evalData(data : List[ast.Datum]) { 
    for(datum <- data) {
      try {
        println("res: " + ExtractExpressions(datum))
      }
      catch {
        case malformed : MalformedExpressionException =>
          println("malformed: " + malformed.getMessage)
        case badspecial : BadSpecialFormException =>
          println("bad special form: " + badspecial.getMessage)
        case unbound : UnboundVariableException =>
          println("unbound variable: " + unbound.getMessage)
      }
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
      
      case ":primitive" =>
        acceptInput(new PrimitiveExpressionMode)

      case userString =>
        mode.evaluate(userString)
        acceptInput(mode)
    }
  }

  def apply() {
    val reader = new ConsoleReader;
    acceptInput(new PrimitiveExpressionMode)(reader)
  }
}
